/* 
 * PostInit.c --
 *
 * Routine to Initialise Postmortem commands
 */

#include "tcl.h"
#include "string.h"

/*
 *    Include default definitions
 */
extern int main();
int *tclDummyMainPtr = (int *) main;

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

extern int post_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;
extern int iocmd_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  static char post_initCmd[] = 
    "set Xpost 0 ; source /mrao/post/src/tcl_test/post.tcl";

    /* 
     * Setup Tcl Interpreter
     */
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /* 
     * Setup Postmortem commands
     */
    if (post_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }

    /* 
     * Setup IO commands
     */
    if (iocmd_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }

    /* 
     * Setup Graphic commands
     */
    if (graphic_Init( interp ) == TCL_ERROR) {
	return TCL_ERROR; 
    }

    /*
     * Perform postmortem specific initialisation
     */
    if (Tcl_Eval(interp, post_initCmd) == TCL_ERROR) {
        return TCL_ERROR;
    }  
    tcl_RcFileName = "~/.tclrc";

    return TCL_OK;

}








