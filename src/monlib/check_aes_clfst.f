

*$(2a)  Auxiliary routines applicable to the CLFST

*+CHECK_AES_CLFST

       subroutine check_aes_clfst (ifile, nae_bad, status)
C      --------------------------------------------------
C
C  Checks aerial status and reports problems.
C
C  Returned:
C      IFILE     integer     sample file logical unit number
C      NAE_BAD   integer     number of problems found
C      STATUS    integer     status value
C
C  Routine to check the aerial status words and report any problems
C  to the output device.  The number of problem aerials (showing
C  gross or noisy pointing errors) is returned.  The following status
C  bits are maintained during the observing run and are investigated
C  by this routine:
C
C  Hut status word,    bit 15 : set if hut on-line and operational
C                      bit 13 : set if aerials tracking
C
C  Aerial status word, bit 12 : set if gross error in Dec
C                      bit 11 : set if gross error in RA
C                      bit 10 : set if Dec pointing noisy
C                      bit 9  : set if RA pointing noisy
C
C  The STATUS value should be zero on entry and is not changed.
C
*-
       INTEGER  IFILE, NAE_BAD, STATUS
C
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
C
       INTEGER  ILIST(MAX_AES), IAE, IAE1, IAE2
       INTEGER  I, IHUT, IOUT, N
C
       IF (STATUS.NE.0) RETURN
C
       NAE_BAD=0
       CALL io_enqout(IOUT)
C
C  Check for gross pointing errors
C
       N=0
       DO IHUT=1,MAX_HUTS
         IF (BTEST(IHUTSTAT(IHUT),15).NE.0 .AND.
     :       BTEST(IHUTSTAT(IHUT),13).NE.0) THEN
           CALL ENQ_AE_HUT(IFILE,IHUT,IAE1,IAE2,STATUS)
           DO IAE=IAE1,IAE2
             IF (BTEST(IAESTAT(IAE),12).NE.0 .OR.
     :           BTEST(IAESTAT(IAE),10).NE.0) THEN
               ILIST(N+1)=IAE
               N=N+1
             ENDIF
           ENDDO
         ENDIF
       ENDDO
       NAE_BAD=NAE_BAD+N
C
       IF (N.GT.0) THEN
         WRITE(IOUT,'(X,A/(16I4))')
     :     'Gross pointing error detected, aerial:',(ILIST(I),I=1,N)
         WRITE(IOUT,*)
       ENDIF
C
C  Check for noisy or slow aerials
C
       N=0
       DO IHUT=1,MAX_HUTS
         IF (BTEST(IHUTSTAT(IHUT),15).NE.0 .AND.
     :       BTEST(IHUTSTAT(IHUT),13).NE.0) THEN
           CALL ENQ_AE_HUT(IFILE,IHUT,IAE1,IAE2,STATUS)
           DO IAE=IAE1,IAE2
             IF (BTEST(IAESTAT(IAE),11).NE.0 .OR.
     :           BTEST(IAESTAT(IAE),9).NE.0) THEN
               ILIST(N+1)=IAE
               N=N+1
             ENDIF
           ENDDO
         ENDIF
       ENDDO
       NAE_BAD=NAE_BAD+N
C
       IF (N.GT.0) THEN
         WRITE(IOUT,'(X,A/(16I4))')
     :     'Noisy or slow aerial detected:',(ILIST(I),I=1,N)
         WRITE(IOUT,*)
       ENDIF
C
       END
