/* 
 * XpostInit.c --
 *
 *   Initialise the Postmortem package commands for use in X-window 
 *   Applications
 *
 */



#include "tk.h"
#include "string.h"

#include "tclRawTCP.h"
#include "tkXAccess.h"

/*
 * The following variable is a special hack that allows applications
 * to be linked using the procedure "main" from the Tk library.  The
 * variable generates a reference to "main", which causes main to
 * be brought in from the library (and all of Tk and Tcl with it).
 */

extern int main();
int *tclDummyMainPtr = (int *) main;



/*
 *---------------------------------------------------------------------
 *   Postmortem initialisation commands
 *
 */
extern int post_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;
extern int iocmd_Init _ANSI_ARGS_(( Tcl_Interp *interp )) ;
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

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  static char Steal_initCmd[] = "source /soft/tcl/v7.3/lib/tk/tkSteal.tcl";
  static char post_initCmd[] =
     "set Xpost 1 ; source /mrao/post/src/tcl_test/post.tcl";

  Tk_Window main;
  
  main = Tk_MainWindow(interp);


    /* 
     * Setup Tcl /Tk Interpreters
     */  
    if (Tcl_Init(interp) == TCL_ERROR) {
      return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
      return TCL_ERROR;
    }


    /*
     * Create commands for use of application imbedding
     */
      create_tclRawTCP();
      if (TkXAccess_Init(interp) == TCL_ERROR)
        return TCL_ERROR;

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
  if (Tcl_Eval(interp, Steal_initCmd) == TCL_ERROR) {
    return TCL_ERROR;
  } 
  if (Tcl_Eval(interp, anmap_initCmd) == TCL_ERROR) {
    return TCL_ERROR;
  }  

  tcl_RcFileName = "~/.wishrc";

  return TCL_OK;
}




















