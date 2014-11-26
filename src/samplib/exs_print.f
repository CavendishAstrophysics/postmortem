C+
       SUBROUTINE EXS_PRINT (OPTION, STATUS)
C      -------------------------------------
C
C  Prints observing parameters on the output device.
C
C  The STATUS value should be zero on entry.
C
*-
       CHARACTER  OPTION*(*)
       INTEGER    STATUS
C
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
       IF (CT_VERS.EQ.0) THEN
         CALL EXS_PRINT_CLFST(OPTION,STATUS)
       ELSEIF (CT_VERS.EQ.2) THEN
         CALL EXS_PRINT_RYLE(OPTION,STATUS)
       ELSE
         CALL SMP_WRERR(ILL_CONTTAB,' ')
       ENDIF
C
       END
