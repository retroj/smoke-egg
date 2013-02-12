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
 srfi-4
 (only srfi-13 string-join)
 srfi-69
 coops
 extras
 foreigners
 (only list-utils assoc-def)
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

(define-foreign-record-type Method
  (Index classId Method-classId)
  (Index name Method-name)
  (Index args Method-args)
  (unsigned-char numArgs Method-numArgs)
  (unsigned-short flags Method-flags)
  (Index ret Method-ret)
  (Index method Method-method))

(define Method-protected?
  (foreign-lambda* bool ((Method meth))
    "C_return(meth->flags & Smoke::mf_protected);"))

(define Method-const?
  (foreign-lambda* bool ((Method meth))
    "C_return(meth->flags & Smoke::mf_const);"))

(define smoke-method
  (foreign-lambda* Method ((Smoke smoke) (Index methidx))
    "Smoke::Method m = smoke->methods[methidx];"
    "C_return(&m);"))

(define smoke-method-name
  (foreign-lambda* c-string ((Smoke smoke) (Method meth))
    "C_return(smoke->methodNames[meth->name]);"))

(define smoke-type-name
  (foreign-lambda* c-string ((Smoke smoke) (Index idx))
    "C_return(smoke->types[idx].name);"))



;;;
;;; Stack
;;;

(define-foreign-type Stack c-pointer)

(define-record smoke-stack
  stack
  size)

(define %make-smoke-stack make-smoke-stack)

(define (make-smoke-stack size)
  (define %make-stack
    (foreign-lambda* Stack ((size_t size))
      "Smoke::StackItem *s = (Smoke::StackItem*)malloc(size * sizeof(Smoke::StackItem));"
      "C_return(s);"))
  (let ((s (%make-stack size)))
    (set-finalizer! s free)
    (%make-smoke-stack s size)))

(define %smoke-stack-int
  (foreign-lambda* int ((Stack stack) (size_t idx))
    "Smoke::Stack s = (Smoke::Stack)stack;"
    "C_return(s[idx].s_int);"))

(define (smoke-stack-int stack idx)
  (%smoke-stack-int (smoke-stack-stack stack) idx))

(define %smoke-stack-pointer
  (foreign-lambda* c-pointer ((Stack stack) (size_t idx))
    "Smoke::Stack s = (Smoke::Stack)stack;"
    "C_return(s[idx].s_voidp);"))

(define (smoke-stack-pointer stack idx)
  (%smoke-stack-pointer  (smoke-stack-stack stack) idx))

(define (%smoke-stack-set-int-pointer! stack idx n)
  (let-location ((n int n))
    (%smoke-stack-set-pointer! stack idx (location n))))

(define (smoke-stack-set-int-pointer! stack idx n)
  (%smoke-stack-set-int-pointer! (smoke-stack-stack stack) idx n))

(define %smoke-stack-set-pointer!
  (foreign-lambda* void
      ((Stack stack) (size_t idx) (c-pointer p))
    "Smoke::Stack s = (Smoke::Stack)stack;"
    "s[idx].s_voidp = p;"))

(define (smoke-stack-set-pointer! stack idx p)
  (%smoke-stack-set-pointer! (smoke-stack-stack stack) idx p))

(define %smoke-stack-set-unsigned-long!
  (foreign-lambda* void
      ((Stack stack) (size_t idx) (unsigned-long n))
    "Smoke::Stack s = (Smoke::Stack)stack;"
    "s[idx].s_ulong = n;"))

(define (smoke-stack-set-unsigned-long! stack idx n)
  (%smoke-stack-set-unsigned-long! (smoke-stack-stack stack) idx n))

(define smoke-stack-setters
  `((c-pointer         . ,%smoke-stack-set-pointer!)
    ((c-pointer int)   . ,%smoke-stack-set-int-pointer!)
    ;; (bool           . )
    ;; (char           . )
    ;; (unsigned-char  . )
    ;; (short          . )
    ;; (unsigned-short . )
    ;; (int            . )
    ;; (unsigned-int   . )
    ;; (long           . )
    (unsigned-long     . ,%smoke-stack-set-unsigned-long!)
    ;; (float          . )
    ;; (double         . )
    ))

(define (smoke-stack-populate! stack vals)
  (let ((s (smoke-stack-stack stack))
        (nvals (length vals))
        (nstack (- (smoke-stack-size stack) 1))
        (i 1))
    (when (> nvals nstack)
      (error (sprintf "smoke-stack not big enough: size ~A, need ~A"
                      nstack nvals)))
    (for-each
     (lambda (x)
       (let* ((type (car x))
              (val (cadr x))
              (setter (cdr (assoc-def type smoke-stack-setters))))
         (setter s i val)
         (set! i (+ 1 i))))
     vals)
    stack))


#>
/* These forward declarations are redundant of what define-external
 * produces, but no matter where the define-external calls are in the
 * file, their declarations are generated after this code block, but
 * we need them up here.
 */
C_externexport void SchemeSmokeBinding_deleted_cb(void*, short int, void*);
C_externexport void SchemeSmokeBinding_callMethod_cb(void*, short int, void*, void*, int);

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
        if (can_callback) {
            SchemeSmokeBinding_callMethod_cb(this, method, obj, args, isAbstract);
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


(define (SchemeSmokeBinding-callMethod this methidx obj stack abstract?)
  (let ((c (hash-table-ref bindings (pointer->address this))))
    (handle-callback c methidx obj stack abstract?)))

(define-external (SchemeSmokeBinding_callMethod_cb
                  (c-pointer this) (Index methidx) (c-pointer obj)
                  (Stack stack) (bool abstract?))
  void (SchemeSmokeBinding-callMethod this methidx obj stack abstract?))


(define (SchemeSmokeBinding-className obj idx)
  ((foreign-lambda* c-string ((c-pointer obj) (Index idx))
     "SchemeSmokeBinding *o = (SchemeSmokeBinding *)obj;"
     "C_return(o->className(idx));")
   obj idx))

(define event-handlers (make-hash-table))

(define (add-event-map obj event)
  (let ((eventmap (make-hash-table initial: '())))
    (hash-table-set! event-handlers obj eventmap)
    eventmap))

(define (add-event-handler obj event handler)
  (let ((eventmap (or (hash-table-ref/default event-handlers obj #f)
                      (add-event-map obj event))))
    (hash-table-update!
     eventmap event (lambda (lst) (cons handler lst)))))

(define-generic (destructor this))
(define-generic (deleted-callback this))
(define-generic (handle-callback this))
(define-generic (find-class this))
(define-generic (find-method this))
(define-generic (instantiate this))

(define-class <SchemeSmokeBinding> ()
  ((this)
   (smoke initform: (error "'smoke field required"))
   (stack #f)
   (initial-stack-size initform: 3)))

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

(define-method (handle-callback (this <SchemeSmokeBinding>) methidx obj stack abstract?)
  (and-let* ((eventmap (hash-table-ref/default event-handlers obj #f))
             (smoke (slot-value this 'smoke))
             (meth (smoke-method smoke methidx))
             (name ((foreign-lambda* c-string ((Smoke smoke) (Method meth))
                      "C_return(smoke->methodNames[meth->name]);")
                    smoke meth))
             (handlers (hash-table-ref/default eventmap name #f)))
    (for-each
     (lambda (handler)
       (handler this methidx obj stack abstract?))
     handlers)))

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

(define (get-stack/create this #!optional (minsize 1))
  (let ((stack (slot-value this 'stack)))
    (unless (and stack (>= (smoke-stack-size stack) minsize))
      (set! (slot-value this 'stack)
            (make-smoke-stack
             (max minsize (slot-value this 'initial-stack-size)))))
    (slot-value this 'stack)))

(define-method (instantiate (this <SchemeSmokeBinding>) cname mname
                            #!optional (args '()))
  (let ((cid (find-class this cname))
        (mid (find-method this cname mname))
        (stack (if (smoke-stack? args)
                   args
                   (smoke-stack-populate!
                    (get-stack/create this (max 2 (length args)))
                    args))))
    (call-method this mid #f stack)
    (let* ((o (smoke-stack-pointer stack 0)))
      (smoke-stack-set-pointer! stack 1 (slot-value this 'this))
      (call-method/classid+methidx this cid 0 o stack)
      o)))


(define (click-test-handler this methidx obj stack abstract?)
  (let* ((smoke (slot-value this 'smoke))
         (meth (smoke-method smoke methidx))
         (protected? (Method-protected? meth))
         (const? (Method-const? meth))
         (mname (smoke-method-name smoke meth))
         (nargs (char->integer (Method-numArgs meth)))
         (argsvector (make-s16vector nargs)))
    ((foreign-lambda* void ((Smoke smoke) (Method meth) (s16vector argsvector) (int nargs))
       "Smoke::Index *idx = smoke->argumentList + meth->args;"
       "size_t i;"
       "for (i = 0; i < nargs; i++) {"
       "    argsvector[i] = idx[i];"
       "}")
     smoke meth argsvector nargs)
    (let ((name (sprintf "~A~A~A(~A)"
                         (if protected? "protected " "")
                         (if const? "const " "")
                         mname
                         (string-join
                          (map (lambda (x) (smoke-type-name smoke x))
                               (s16vector->list argsvector))
                          ", "))))
      (printf "~A(~A)::~A~%"
              (SchemeSmokeBinding-className
               (slot-value this 'this)
               (Method-classId meth))
              obj
              name))))


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

(define (call-method binding methId thisobj #!optional (args '()))
  (let ((stack (if (smoke-stack? args)
                   args
                   (smoke-stack-populate!
                    (get-stack/create binding (max 1 (length args)))
                    args))))
    ((%call-method-form foreign-lambda* "0")
     binding methId
     (if (eq? #f thisobj)
         (foreign-value "((void*)0)" c-pointer)
         thisobj)
     (smoke-stack-stack stack))))

(define (call-method-with-callbacks binding methId thisobj #!optional (args '()))
  (let ((stack (if (smoke-stack? args)
                   args
                   (smoke-stack-populate!
                    (get-stack/create binding (max 1 (length args)))
                    args))))
    ((%call-method-form foreign-safe-lambda* "1")
     binding methId
     (if (eq? #f thisobj)
         (foreign-value "((void*)0)" c-pointer)
         thisobj)
     (smoke-stack-stack stack))))

(define (call-method/classid+methidx binding classid methidx thisobj #!optional (args '()))
  (let ((stack (if (smoke-stack? args)
                   args
                   (smoke-stack-populate!
                    (get-stack/create binding (max 1 (length args)))
                    args))))
    ((foreign-lambda* void
         ((SchemeSmokeBinding binding) (ModuleIndex classId) (Index methodIdx)
          (c-pointer thisobj) (Stack stack))
       "Smoke::ClassFn fn = classId->smoke->classes[classId->index].classFn;"
       "Smoke::Method* m = classId->smoke->methods + methodIdx;"
       "fn(m->method, thisobj, (Smoke::Stack)stack);")
     binding classid methidx thisobj (smoke-stack-stack stack))))

)
