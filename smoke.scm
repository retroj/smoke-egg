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
 cplusplus-object
 extras)

(foreign-declare "#include <smoke.h>")
#>
#include <smoke/qtcore_smoke.h>
#include <smoke/qtgui_smoke.h>

#include <iostream>
#include <string>
#include <stdio.h>
 
using namespace std;

/*
 * This class will intercept all virtual method calls and will get
 * notified when an instance created by smoke gets destroyed.
 */
class MySmokeBinding : public SmokeBinding
{
public:
    MySmokeBinding(Smoke *s) : SmokeBinding(s) {}
 
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
 
int themain ()
{
    int argc = 0;
    char **argv;

    // init the Qt SMOKE runtime
    //init_qtcore_Smoke();
    //init_qtgui_Smoke();
 
    // create a SmokeBinding for the Qt SMOKE runtime
    MySmokeBinding qtcoreBinding(qtcore_Smoke);
    MySmokeBinding qtguiBinding(qtgui_Smoke);
 
    // find the 'QApplication' class
    Smoke::ModuleIndex classId = qtcore_Smoke->findClass("QApplication");
    /* find the methodId. we use a munged method signature, where 
     * $ is a plain scalar
     * # is an object
     * ? is a non-scalar (reference to array or hash, undef) */
    Smoke::ModuleIndex methId = classId.smoke->findMethod("QApplication",
        "QApplication$?");  // find the constructor
    printf("QApplication classId: [%s, %d], QApplication($?) methId: [%s, %d]\n",
           classId.smoke->moduleName(),
           classId.index,
           methId.smoke->moduleName(),
           methId.index);
 
    // get the Smoke::Class
    Smoke::Class klass = classId.smoke->classes[classId.index];
 
    // findMethod() returns an index into methodMaps, which has
    // information about the classId, methodNameId and methodId. we 
    // are interested in the methodId to get a Smoke::Method
    Smoke::Method meth = methId.smoke->methods[methId.smoke->methodMaps[methId.index].method];
 
    Smoke::StackItem stack[3];
    // QApplication expects a reference to argc, so we pass it as a pointer
    stack[1].s_voidp = &argc;
    stack[2].s_voidp = argv;
    // call the constructor, Smoke::Method::method is the methodId
    // specifically for this class.
    (*klass.classFn)(meth.method, 0, stack);
 
    // the zeroth element contains the return value, in this case the
    // QApplication instance
    void *qapp = stack[0].s_voidp;
 
    // method index 0 is always "set smoke binding" - needed for
    // virtual method callbacks etc.
    stack[1].s_voidp = &qtguiBinding;
    (*klass.classFn)(0, qapp, stack);
 
    // create a widget
    classId = qtcore_Smoke->findClass("QWidget");
    methId = classId.smoke->findMethod("QWidget", "QWidget");
    printf("QWidget classId: [%s, %d], QWidget() methId: [%s, %d]\n",
           classId.smoke->moduleName(),
           classId.index,
           methId.smoke->moduleName(),
           methId.index);
 
    klass = classId.smoke->classes[classId.index];
    meth = methId.smoke->methods[methId.smoke->methodMaps[methId.index].method];
 
    (*klass.classFn)(meth.method, 0, stack);
    void *widget = stack[0].s_voidp;
    // set the smoke binding
    stack[1].s_voidp = &qtguiBinding;
    (*klass.classFn)(0, widget, stack);
 
    // show the widget
    methId = classId.smoke->findMethod("QWidget", "show");
    printf("QWidget classId: [%s, %d], show() methId: [%s, %d]\n",
           classId.smoke->moduleName(),
           classId.index,
           methId.smoke->moduleName(),
           methId.index);
    meth = methId.smoke->methods[methId.smoke->methodMaps[methId.index].method];
    (*klass.classFn)(meth.method, widget, 0);
 
    // we don't even need findClass() when we use the classId provided
    // by the MethodMap
    methId = qtgui_Smoke->findMethod("QApplication", "exec");
    printf("QApplication classId: %d, exec() methId: [%s, %d]\n",
           qtgui_Smoke->methodMaps[methId.index].classId,
           methId.smoke->moduleName(),
           methId.index);
 
    klass = methId.smoke->classes[methId.smoke->methodMaps[methId.index].classId];
    meth = methId.smoke->methods[methId.smoke->methodMaps[methId.index].method];
 
    // call QApplication::exec()
    (*klass.classFn)(meth.method, 0, stack);  
 
    // store the return value of QApplication::exec()
    int retval = stack[0].s_int;
 
    // destroy the QApplication instance
    methId = qtgui_Smoke->findMethod("QApplication", "~QApplication");
    printf("QApplication classId: %d, ~QApplication() methId: [%s, %d]\n",
           qtgui_Smoke->methodMaps[methId.index].classId,
           methId.smoke->moduleName(),
           methId.index);
    meth = methId.smoke->methods[methId.smoke->methodMaps[methId.index].method];
    (*klass.classFn)(meth.method, qapp, 0);
 
    // destroy the smoke instance
    //delete qtcore_Smoke;
    //delete qtgui_Smoke;
 
    // return the previously stored value
    return retval;
}


<#

(use smoke-qtcore)
(use smoke-qtgui)

(printf "smoke-egg: started~%")

(init-qtcore-smoke)
(init-qtgui-smoke)
((foreign-lambda int themain))
(delete-qtcore-smoke)
(delete-qtgui-smoke)

'(define-foreign-type Smoke c-pointer)


;; struct ModuleIndex {
;;     Smoke* smoke;
;;     Index index;
;; };


'(define (find-method smoke class method)
  ;; returns ModuleIndex object
  )

;; #>
;; // Find the method's index
;; Smoke::ModuleIndex mi = smoke->findMethod( "QLabel", "QLabel$#$" );
;; Smoke::Index methodIdx;
;; if ( mi.index > 0 ) {
;;     methodIdx = smoke->methodMaps[mi.index].method;
;; }
;; else {
;;     // Resolve ambiguous method call
;; }

;; // Construct the argument array
;; Smoke::StackItem stack[4];
;; stack[1].s_voidp = (void*)("Hello, World!");
;; // A null value for the parent will construct a top-level widget
;; stack[2].s_voidp = 0;
;; // Qt::Dialog is declared in the Qt::WindowType enum and WindowFlags type is a typedef for QFlags<WindowType>. 
;; stack[3].s_enum = Qt::Dialog;

;; // Call the method
;; Smoke::Method* m = qt_Smoke->methods + methodIdx;
;; Smoke::ClassFn fn = qt_Smoke->classes[m->classId].classFn;
;; fn(m->method, 0, stack);

;; // Get the return value
;; QLabel* label = (QLabel*)stack[0];
;; <#


;; (define-class <Point> (<c++-object>) ())

;; (declare (hide g0))
;; (define g0
;;   (foreign-lambda void "delete " (c-pointer "Point")))
;; (define-method (destructor (this <Point>))
;;   (g0 (slot-value this 'this)))

;; (declare (hide g1))
;; (define g1
;;   (foreign-lambda (c-pointer "Point") "new Point" integer integer))
;; (define-method
;;   (constructor (this <Point>) initargs)
;;   (set! (slot-value this 'this) (##sys#apply g1 initargs)))

;; (declare (hide g2))
;; (define g2
;;   (foreign-lambda*
;;       void
;;       (((c-pointer "Point") g3) (integer g4) (integer g5))
;;     "g3->shift(g4,g5);"))
;; (define-method
;;   (shift (this <Point>) #!rest args)
;;   (##sys#apply g2 (slot-value this 'this) args))

;; (declare (hide g6))
;; (define g6
;;   (foreign-lambda*
;;       "string"
;;       (((c-pointer "Point") g7))
;;     "return(g7->toString());"))
;; (define-method
;;   (toString (this <Point>) #!rest args)
;;   (##sys#apply g6 (slot-value this 'this) args))


'(define-foreign-type StackItem (union StackItem))

'(define-foreign-type Stack (c-pointer (union StackItem)))

'(define-foreign-type EnumOperation (enum EnumOperation))

#|
    typedef void (*ClassFn)(Index method, void* obj, Stack args);
    typedef void* (*CastFn)(void* obj, Index from, Index to);
    typedef void (*EnumFn)(EnumOperation, Index, void*&, long&);

    struct Class {
	const char *className;	// Name of the class
	bool external;		// Whether the class is in another module
	Index parents;		// Index into inheritanceList
	ClassFn classFn;	// Calls any method in the class
	EnumFn enumFn;		// Handles enum pointers
        unsigned short flags;   // ClassFlags
        unsigned int size;
    };
|#

)
