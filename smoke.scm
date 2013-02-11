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
 srfi-69
 coops
 extras
 foreigners
 lolevel)

(foreign-declare "#include <smoke.h>")

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


#>
#include <iostream>
#include <string>

using namespace std;

/* These forward declarations are redundant of what define-external
 * produces, but no matter where the define-external calls are in the
 * file, their declarations are generated after this code block, but
 * we need them up here.
 */
C_externexport void SchemeSmokeBinding_deleted_cb(void*, short int, void*);
C_externexport void SchemeSmokeBinding_callMethod_cb(void*, short int, short int,
                                                     void*, void*, int);

/*
 * This class will intercept all virtual method calls and will get
 * notified when an instance created by smoke gets destroyed.
 */
class SchemeSmokeBinding : public SmokeBinding
{
public:
    bool can_callback;

    SchemeSmokeBinding(Smoke *s) : SmokeBinding(s) {
        can_callback = 0;
    }

    void deleted(Smoke::Index classId, void *obj) {
        SchemeSmokeBinding_deleted_cb(this, classId, obj);
    }

    bool callMethod(Smoke::Index method, void *obj,
        Smoke::Stack args, bool isAbstract)
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

        if (name == "protected mousePressEvent(QMouseEvent*)") {
            cout << className(meth.classId) << "(" << obj
                 << ")::" << name << endl;
            if (can_callback) {
                SchemeSmokeBinding_callMethod_cb(this, meth.classId, method,
                                                 obj, args, isAbstract);
            }
        }
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

(declare (hide bindings))
(define bindings (make-hash-table))

(define (SchemeSmokeBinding-deleted this idx obj)
  (let ((c (hash-table-ref bindings (pointer->address this))))
    (deleted-callback c idx obj)))

(define-external (SchemeSmokeBinding_deleted_cb
                  (c-pointer this) (Index classidx) (c-pointer obj))
  void (SchemeSmokeBinding-deleted this classidx obj))


(define (SchemeSmokeBinding-callMethod this classidx methidx
                                       obj stack abstract?)
  (let ((c (hash-table-ref bindings (pointer->address this))))
    (handle-callback c classidx methidx obj stack abstract?)))

(define-external (SchemeSmokeBinding_callMethod_cb
                  (c-pointer this) (Index classidx) (Index methidx)
                  (c-pointer obj) (Stack stack) (bool abstract?))
  void (SchemeSmokeBinding-callMethod this classidx methidx
                                      obj stack abstract?))


(define (SchemeSmokeBinding-className obj idx)
  ((foreign-lambda* c-string ((c-pointer obj) (Index idx))
     "SchemeSmokeBinding *o = (SchemeSmokeBinding *)obj;"
     "C_return(o->className(idx));")
   obj idx))


(define-generic (destructor this))
(define-generic (deleted-callback this))
(define-generic (handle-callback this))
(define-generic (find-class this))
(define-generic (find-method this))

(define-class <SchemeSmokeBinding> ()
  ((this)
   (smoke initform: (error "'smoke field required"))))

(define-foreign-type SchemeSmokeBinding
  (instance SchemeSmokeBinding <SchemeSmokeBinding>))

(define-method (initialize-instance (this <SchemeSmokeBinding>))
  (call-next-method)
  (set! (slot-value this 'this)
        ((foreign-lambda (c-pointer "SchemeSmokeBinding")
                         "new SchemeSmokeBinding" Smoke)
         (slot-value this 'smoke)))
  (hash-table-set! bindings (pointer->address (slot-value this 'this)) this))

(define-method (destructor (this <SchemeSmokeBinding>))
  ((foreign-lambda void "delete " (c-pointer "SchemeSmokeBinding"))
   (slot-value this 'this)))

(define-method (deleted-callback (this <SchemeSmokeBinding>) idx obj)
  (printf "~A~A (~A)~%" "~"
          (SchemeSmokeBinding-className (slot-value this 'this) idx)
          obj))

(define-method (handle-callback (this <SchemeSmokeBinding>)
                                classidx methidx obj stack abstract?)
  (printf "methodcall: ~A~%"
          (SchemeSmokeBinding-className (slot-value this 'this) classidx)))

(define-method (find-class (this <SchemeSmokeBinding>) cname)
  (define %find-class
    (foreign-lambda* ModuleIndex
        ((Smoke smoke) (c-string cname))
      "Smoke::ModuleIndex classId = smoke->findClass(cname);"
      "Smoke::ModuleIndex *c = (Smoke::ModuleIndex*)malloc(sizeof(Smoke::ModuleIndex));"
      "memcpy(c, &classId, sizeof(Smoke::ModuleIndex));"
      "C_return(c);"))
  (let ((c (%find-class (slot-value this 'smoke) cname)))
    (set-finalizer! c free)
    c))

(define-method (find-method (this <SchemeSmokeBinding>) cname mname)
  (define %find-method
    (foreign-lambda* ModuleIndex
        ((Smoke smoke) (c-string cname) (c-string mname))
      "Smoke::ModuleIndex methId = smoke->findMethod(cname, mname);"
      "Smoke::ModuleIndex *m = (Smoke::ModuleIndex*)malloc(sizeof(Smoke::ModuleIndex));"
      "memcpy(m, &methId, sizeof(Smoke::ModuleIndex));"
      "C_return(m);"))
  (let ((m (%find-method (slot-value this 'smoke) cname mname)))
    (set-finalizer! m free)
    m))


;;;
;;; Call Method
;;;

(define-syntax %call-method-form
  (syntax-rules ()
    ((%call-method-form sym can-callback)
     (sym void
          ((SchemeSmokeBinding binding) (ModuleIndex methId)
           (c-pointer thisobj) (Stack stack))
          "binding->can_callback = " can-callback ";"
          "Smoke::Index methodIdx;"
          "if (methId->index > 0) {"
          "    methodIdx = methId->smoke->methodMaps[methId->index].method;"
          "} else {"
          "    /* Resolve ambiguous method call */"
          "}"
          "Smoke::Method* m = methId->smoke->methods + methodIdx;"
          "Smoke::ClassFn fn = methId->smoke->classes[m->classId].classFn;"
          "fn(m->method, thisobj, (Smoke::Stack)stack);"
          "binding->can_callback = 0;"))))

(define (call-method binding methId thisobj stack)
  ((%call-method-form foreign-lambda* "0")
   binding methId
   (if (eq? #f thisobj)
       (foreign-value "((void*)0)" c-pointer)
       thisobj)
   stack))

(define (call-method/safe binding methId thisobj stack)
  ((%call-method-form foreign-safe-lambda* "1")
   binding methId
   (if (eq? #f thisobj)
       (foreign-value "((void*)0)" c-pointer)
       thisobj)
   stack))

(define (call-method/classid+methidx binding classid methidx thisobj stack)
  ((foreign-lambda* void
       ((SchemeSmokeBinding binding) (ModuleIndex classId) (Index methodIdx)
        (c-pointer thisobj) (Stack stack))
     "Smoke::ClassFn fn = classId->smoke->classes[classId->index].classFn;"
     "Smoke::Method* m = classId->smoke->methods + methodIdx;"
     "fn(m->method, thisobj, (Smoke::Stack)stack);")
   binding classid methidx thisobj stack))

)
