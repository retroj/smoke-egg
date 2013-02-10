;; Copyright 2013 John J Foerch. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module smoke
    *

(import chicken scheme foreign)

(use
 coops
 extras
 foreigners
 lolevel)

(foreign-declare "#include <smoke.h>")


#>
#include <iostream>
#include <string>

using namespace std;

/*
 * This class will intercept all virtual method calls and will get
 * notified when an instance created by smoke gets destroyed.
 */
class SchemeSmokeBinding : public SmokeBinding
{
public:
    SchemeSmokeBinding(Smoke *s) : SmokeBinding(s) {}

    void deleted(Smoke::Index classId, void *obj) {
        printf("~%s (%p)\n", className(classId), obj);
    }

    bool callMethod(Smoke::Index method, void *obj,
        Smoke::Stack /*args*/, bool /*isAbstract*/)
    {
        Smoke::Method meth = smoke->methods[method];
        string name;

        // check for method flags
        if (meth.flags & Smoke::mf_protected) name += "protected ";
        if (meth.flags & Smoke::mf_const) name += "const ";

        // add the name
        name += smoke->methodNames[meth.name] + string("(");

        // iterate over the argument list and build up the
        // parameter names
        Smoke::Index *idx = smoke->argumentList + meth.args;
        while (*idx) {
            name += smoke->types[*idx].name;
            idx++;
            if (*idx) name += ", ";
        }
        name += ")";

        if (name == "protected mousePressEvent(QMouseEvent*)")
            cout << className(meth.classId) << "(" << obj
                 << ")::" << name << endl;
        return false;
    }

    /*
     * In a bindings runtime, this should return the classname as used
     * in the bindings language, e.g. Qt::Widget in Ruby or
     * Qyoto.QWidget in C#
     */
    char *className(Smoke::Index classId) {
        return (char*) smoke->classes[classId].className;
    }
};
<#

(define-generic (destructor this))

(define-class <SchemeSmokeBinding> ()
  ((this)
   (smoke initform: (error "'smoke field required"))))

(define-method (initialize-instance (this <SchemeSmokeBinding>))
  (call-next-method)
  (set! (slot-value this 'this)
        ((foreign-lambda (c-pointer "SchemeSmokeBinding")
                         "new SchemeSmokeBinding" Smoke)
         (slot-value (slot-value this 'smoke) 'this))))

(define-method (destructor (this <SchemeSmokeBinding>))
  ((foreign-lambda void "delete " (c-pointer "SchemeSmokeBinding"))
   (slot-value this 'this)))



(define-foreign-type Index short)

(define-class <Smoke> ()
  ((this)))

(define-foreign-type Smoke (instance Smoke <Smoke>))

(define smoke-modulename
  (foreign-lambda* c-string ((Smoke smoke))
    "C_return(smoke->moduleName());"))


(define-foreign-record-type ModuleIndex
  (Smoke smoke ModuleIndex-smoke)
  (Index index ModuleIndex-index))

(define (find-method smoke class method)
  (define %find-method
    (foreign-lambda* ModuleIndex
        ((Smoke smoke) (c-string cname) (c-string mname))
      "Smoke::ModuleIndex methId = smoke->findMethod(cname, mname);"
      "Smoke::ModuleIndex *m = (Smoke::ModuleIndex*)malloc(sizeof(Smoke::ModuleIndex));"
      "memcpy(m, &methId, sizeof(Smoke::ModuleIndex));"
      "C_return(m);"))
  (let ((m (%find-method smoke class method)))
    (set-finalizer! m free)
    m))

(define (find-class smoke class)
  (define %find-class
    (foreign-lambda* ModuleIndex
        ((Smoke smoke) (c-string cname))
      "Smoke::ModuleIndex classId = smoke->findClass(cname);"
      "Smoke::ModuleIndex *c = (Smoke::ModuleIndex*)malloc(sizeof(Smoke::ModuleIndex));"
      "memcpy(c, &classId, sizeof(Smoke::ModuleIndex));"
      "C_return(c);"))
  (let ((c (%find-class smoke class)))
    (set-finalizer! c free)
    c))


;;;
;;; Stack
;;;

(define-foreign-type Stack c-pointer)

(define (make-stack size)
  (define %make-stack
    (foreign-lambda* Stack ((size_t size))
      "Smoke::StackItem *s = (Smoke::StackItem*)malloc(size * sizeof(Smoke::StackItem));"
      "C_return(s);"))
  (let ((s (%make-stack size)))
    (set-finalizer! s free)
    s))

(define stack-int
  (foreign-lambda* int ((Stack stack) (size_t idx))
    "Smoke::Stack s = (Smoke::Stack)stack;"
    "C_return(s[idx].s_int);"))

(define stack-pointer
  (foreign-lambda* c-pointer ((Stack stack) (size_t idx))
    "Smoke::Stack s = (Smoke::Stack)stack;"
    "C_return(s[idx].s_voidp);"))

(define (stack-set-int-pointer! stack idx n)
  (let-location ((n int n))
    (stack-set-pointer! stack idx (location n))))

(define stack-set-pointer!
  (foreign-lambda* void
      ((Stack stack) (size_t idx) (c-pointer p))
    "Smoke::Stack s = (Smoke::Stack)stack;"
    "s[idx].s_voidp = p;"))

(define stack-set-unsigned-long!
  (foreign-lambda* void
      ((Stack stack) (size_t idx) (unsigned-long l))
    "Smoke::Stack s = (Smoke::Stack)stack;"
    "s[idx].s_ulong = l;"))


;;;
;;; Call Method
;;;

(define (call-method smoke methId thisobj stack)
  ((foreign-lambda* void
       ((Smoke smoke) (ModuleIndex methId) (c-pointer thisobj) (Stack stack))
     "Smoke::Index methodIdx;"
     "if (methId->index > 0) {"
     "    methodIdx = methId->smoke->methodMaps[methId->index].method;"
     "} else {"
     "    /* Resolve ambiguous method call */"
     "}"
     "Smoke::Method* m = methId->smoke->methods + methodIdx;"
     "Smoke::ClassFn fn = methId->smoke->classes[m->classId].classFn;"
     "fn(m->method, thisobj, (Smoke::Stack)stack);"
     )
   smoke methId
   (if (eq? #f thisobj)
       (foreign-value "((void*)0)" c-pointer)
       thisobj)
   stack))

(define (call-method/classid+methidx smoke classid methidx thisobj stack)
  ((foreign-lambda* void
       ((Smoke smoke) (ModuleIndex classId) (Index methodIdx)
        (c-pointer thisobj) (Stack stack))
     "Smoke::ClassFn fn = smoke->classes[classId->index].classFn;"
     "Smoke::Method* m = smoke->methods + methodIdx;"
     "fn(m->method, thisobj, (Smoke::Stack)stack);")
   smoke classid methidx thisobj stack))

)
