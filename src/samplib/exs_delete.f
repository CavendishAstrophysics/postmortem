C+
       SUBROUTINE EXS_DELETE (FILE, MSAVE, INTER, STATUS)
C      --------------------------------------------------
C
C  Routine to delete a sample file from disc.
C
C  MSAVE records whether the file has already been saved.
C  (MSAVE=0 if not saved, =1 if save requested, =2 if saved, =3 if modified)
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER  FILE*(*)
       CHARACTER  COMMAND*64
       INTEGER    MSAVE, STATUS
       LOGICAL    INTER
       INTEGER    ILIST
       INTEGER    M, LF
C
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       INTEGER*4  ISTR(20)
       CHARACTER  STRING*80
       EQUIVALENCE (ISTR,STRING)
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(ILIST)
C
       IF (MSAVE.NE.2) THEN
         IF (MSAVE.EQ.0 .OR. MSAVE.EQ.1) THEN
           STRING='this sample file has not yet been saved,'
         ELSEIF (MSAVE.EQ.3) THEN
           STRING='this sample file has been modified,'
         ENDIF
         M=chr_lenb(STRING)
         STRING(M+1:)=' do you want to delete it? '
         IF (INTER) THEN
           IF (io_yesno(STRING,' ',STATUS)) MSAVE=2
           IF (MSAVE.NE.1 .AND. MSAVE.NE.2) THEN
c            IF (io_yesno('do you want to save it on tape? ',' ',
c    :                                                    STATUS)) THEN
c              CALL EXS_SAVE(FILE,MSAVE,INTER,STATUS)
c            ENDIF
           ENDIF
         ELSE
           WRITE(ILIST,*)STRING(1:39)
         ENDIF
       ENDIF
C
C    Delete the file from disc
C
       IF (MSAVE.EQ.2) THEN
         LF=chr_ilstc(FILE,'/')-1
         command = 'rm -r '//FILE(1:LF)//' >&! /dev/null'
         call io_system(command(1:chr_lenb(command)),status)
         IF (STATUS.EQ.0) THEN
           WRITE(ILIST,*)'... observation files deleted'
         ELSE
           STRING='deleting '//FILE(1:LF)
           CALL IO_WRERR(STATUS,STRING)
           STATUS=0
         ENDIF
       ENDIF
C
       END
