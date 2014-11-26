

C+SET_AERIALS

       subroutine set_aerials ( lun, list, ilist, mae, nae, status )
C
C  Resolves an aerial list into aerial numbers.
C
C  Given:
C      LUN       integer     sample file logical unit number
C      LIST      char*(*)    string containing input aerial list
C      MAE       integer     maximum number of aerials in the list
C
C  Returned:
C      ILIST     integer(*)  array containing aerial numbers
C      NAE       integer     number of aerials in the list
C      STATUS    integer     status value
C
C  Routine to resolve a list of aerials, presented as a character string,
C  into an array containing valid aerial numbers, ordered from East
C  to West (increasing order).  Repeated occurrences of an aerial in the
C  list are removed.
C
C  The status value should be zero on entry.  The returned status value
C  is zero, unless the input string does not contain a valid aerial list
C  (ILL_AELIST).
C
C  Attempt to define an aerial list by HUT for a telescope other than
C  T151 or 38MHZ is faulted.
C
*-
       CHARACTER LIST*(*)
       INTEGER   LUN, MAE, ILIST(MAE), NAE, STATUS
C
       include '/mrao/post/include/clfst_constants.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/samplib_errors.inc'
C
       CHARACTER HUTS*(MAX_HUTS)
       INTEGER   IHUTS(MAX_HUTS), NHUT, ITSCOPE
       INTEGER   NAE_MAX, NSP_MAX, NBA_MAX, NCH_MAX
       INTEGER   NAE_O, NSP_O, NBA_O, NCH_O
       INTEGER   I, IAE, IAE1, IAE2
       LOGICAL   chr_chalfb, chr_chsame, HUTLIST
C
       IF (STATUS.NE.0) RETURN
       CALL ENQ_PHYS_TSCOPE( LUN, ITSCOPE, STATUS )
       IF (STATUS .NE. 0) GOTO 999

       IF (ITSCOPE.EQ.CLFST .or. ITSCOPE.eq.FIVE_KM) then
         NAE=0
         CALL ENQ_OBSDEF(LUN,NAE_O,NSP_O,NBA_O,NCH_O,STATUS)
         CALL ENQ_TELDEF(LUN,NAE_MAX,NSP_MAX,NBA_MAX,NCH_MAX,STATUS)
C
C    List specifies all aerials
C
         IF (chr_chsame(LIST(1:3),'ALL')) THEN
           NAE=MIN(MAE,NAE_O)
           DO I=1,NAE
             CALL ENQ_IAE_CODE(LUN,I,ILIST(I),STATUS)
           ENDDO
           RETURN
         ENDIF
C
C    Interpret hut or aerial prefix
C
         IF (chr_chsame(LIST(1:3),'HUT')) THEN
           IF (ITSCOPE.NE.CLFST) THEN
             STATUS = ILL_AELIST
           END IF
           HUTLIST=.TRUE.
           I=5
         ELSEIF (chr_chsame(LIST(1:2),'AE')) THEN
           HUTLIST = .FALSE.
           I=3
         ELSEIF (chr_chalfb(LIST(1:1))) THEN
           IF (ITSCOPE.NE.CLFST) THEN
             STATUS = ILL_AELIST
           END IF
           HUTLIST=.TRUE.
           I=1
         ELSE
           HUTLIST=.FALSE.
           I=1
         ENDIF
C
C    Aerials specified by hut, decode hut list and include all
C    aerials within each hut.
C
         IF (HUTLIST .AND. STATUS.EQ.0) THEN
           CALL chr_chlstc(LIST(I:),HUTS,IHUTS,MAX_HUTS,NHUT,STATUS)
           IF (STATUS.NE.0) STATUS=ILL_AELIST
           IF (STATUS.EQ.0) THEN
             DO I=1,NHUT
               CALL ENQ_AE_HUT(LUN,IHUTS(I),IAE1,IAE2,STATUS)
               DO IAE=IAE1,IAE2
                 NAE=NAE+1
                 ILIST(NAE)=IAE
               ENDDO
             ENDDO
           ENDIF
C
C    Aerials specified by number
C
         ELSE
           CALL chr_chlsti(LIST(I:),ILIST,MAE,NAE,STATUS)
           IF (STATUS.NE.0) STATUS=ILL_AELIST
         ENDIF
C
C    Order aerials from East to West, reject index numbers out of range
C    and remove repeated indexes.
C
         IF (STATUS.EQ.0) THEN
C
C     sort
C
           CALL util_sorti(ILIST,NAE)
C
C    resolve which aerials are present in the current observation
C
           I = 0
           DO IAE=1,NAE
             DO IAE1=1,NAE_O
               CALL ENQ_IAE_CODE(LUN,IAE1,IAE2,STATUS)
               IF (ILIST(IAE).EQ.IAE2) THEN
                 I = I + 1
                 ILIST(I) = ILIST(IAE)
               END IF
             END DO
           END DO
           NAE = I
C
           IF (NAE.GT.1) THEN
             I=2
             DO IAE=2,NAE
               IF (ILIST(IAE).NE.ILIST(IAE-1)) THEN
                 ILIST(I)=ILIST(IAE)
                 I=I+1
               ENDIF
             ENDDO
             NAE=I-1
           ENDIF
         ENDIF
       ELSE
         STATUS = ILL_TSCOPE
         GOTO 999
       END IF
       RETURN
C
 999   CALL smp_wrerr( STATUS, 'in subroutine SET_AERIALS' )
       END
