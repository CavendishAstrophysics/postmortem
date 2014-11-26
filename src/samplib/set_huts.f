

C+SET_HUTS

       subroutine set_huts ( lun, list, ilist, mhut, nhut, status )
C
C  Resolves a hut list into index numbers.
C
C  Given:
C      LUN       integer     sample file logical unit number
C      LIST      char*(*)    string containing input hut list
C      MHUT      integer     maximum number of huts in the list
C
C  Returned:
C      ILIST     integer(*)  array containing hut index numbers
C      NHUT      integer     number of huts in the list
C      STATUS    integer     status value
C
C  Routine to resolve a list of huts, presented as a character string,
C  into an array containing hut index numbers, ordered from East to West
C  (increasing order).  Repeated occurrences of a hut in the list are removed.
C
C  The status value should be zero on entry.  The returned status value
C  is zero, unless the input string does not contain a valid hut list
C  (ILL_HUTLIST).
C
*-
       CHARACTER LIST*(*)
       INTEGER   LUN, MHUT, ILIST(MHUT), NHUT, STATUS
C
       include '/mrao/post/include/clfst_constants.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/samplib_errors.inc'
C
       CHARACTER HUTS*(MAX_HUTS)
       INTEGER   ITSCOPE
       INTEGER   I, IHUT
       LOGICAL   chr_chalfb, chr_chsame
C
       IF (STATUS.NE.0) RETURN
       CALL ENQ_PHYS_TSCOPE( LUN, ITSCOPE, STATUS )
       IF (STATUS .NE. 0) GOTO 999

       IF (ITSCOPE.EQ.CLFST) THEN
         IF (chr_chsame(LIST(1:3),'ALL')) THEN
C          List specifies all huts
           NHUT=MIN(MHUT,MAX_HUTS)
           DO I=1,NHUT
             ILIST(I)=I
           ENDDO
         ELSE
C          Interpret hut prefix
           IF (chr_chsame(LIST(1:3),'HUT')) THEN
             I=5
           ELSEIF (chr_chalfb(LIST(1:1))) THEN
             I=1
           ELSE
             STATUS=ILL_HUTLIST
           ENDIF

C          Decode hut list
           NHUT=0
           CALL chr_chlstc(LIST(I:),HUTS,ILIST,MAX_HUTS,NHUT,STATUS)
           IF (STATUS.NE.0) STATUS=ILL_HUTLIST

C          Order huts from East to West, reject index numbers out of
C          range and remove repeated indexes.
           IF (STATUS.EQ.0) THEN
             CALL util_sorti(ILIST,NHUT)
C
             I=1
             DO IHUT=1,NHUT
               IF (ILIST(IHUT).GE.1 .AND. ILIST(IHUT).LE.MAX_HUTS) THEN
                 ILIST(I)=ILIST(IHUT)
                 I=I+1
               ENDIF
             ENDDO
             NHUT=I-1
C
             IF (NHUT.GT.1) THEN
               I=2
               DO IHUT=2,NHUT
                 IF (ILIST(IHUT).NE.ILIST(IHUT-1)) THEN
                   ILIST(I)=ILIST(IHUT)
                   I=I+1
                 ENDIF
               ENDDO
               NHUT=I-1
             ENDIF
           END IF
         END IF
       ELSE
         STATUS = ILL_TSCOPE
         GOTO 999
       ENDIF

       RETURN
C
 999   call smp_wrerr( status, 'in subroutine SET_HUTS' )

       END
