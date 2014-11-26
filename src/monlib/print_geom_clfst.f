

*+PRINT_GEOM_CLFST

       SUBROUTINE PRINT_GEOM_CLFST (IFILE, STATUS)
C      -------------------------------------------
C
C  Executes the PRINT-GEOMETRY command.
C
C  Given:
C      IFILE         integer       sample file unit number
C      STATUS        integer       status value
C
C  Support routine for PRINT_GEOMETRY, for the CLFST array.
C
*-
       INTEGER    IFILE, STATUS
C
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
C
       CHARACTER  DATE*9
       INTEGER    IOUT, IDATE(3), LENGTH
       INTEGER    I, IAE, IAE1, IAE2, IHUT
C
       IF (STATUS.NE.0) RETURN

       CALL io_enqout(IOUT)
C
       IDATE(1)=IGDAT(1)
       IDATE(2)=IGDAT(2)
       IDATE(3)=IGDAT(3)
       CALL chr_chdate(IDATE,1,DATE,LENGTH)
       WRITE(IOUT,*)'Last revision ',DATE
C
       DO I=1,3
         WRITE(IOUT,*)
         IF (I.EQ.1) WRITE(IOUT,*)'X-coordinate'
         IF (I.EQ.2) WRITE(IOUT,*)'Y-coordinate'
         IF (I.EQ.3) WRITE(IOUT,*)'Z-coordinate'
         DO IHUT=1,MAX_HUTS
           CALL ENQ_AE_HUT(IFILE,IHUT,IAE1,IAE2,STATUS)
           IF (I.EQ.1) WRITE(IOUT,1)(X(IAE),IAE=IAE1,IAE2)
           IF (I.EQ.2) WRITE(IOUT,1)(Y(IAE),IAE=IAE1,IAE2)
           IF (I.EQ.3) WRITE(IOUT,1)(Z(IAE),IAE=IAE1,IAE2)
    1      FORMAT(8F10.2)
         ENDDO
       ENDDO
C
       END
