C+
       SUBROUTINE EXS_SAVE (FILE, MSAVE, INTER, STATUS)
C      ------------------------------------------------
C
C  Routine to mark a sample file on disc for saving on tape.
C
C  MSAVE records whether the file has already been saved.
C  (MSAVE=0 if not saved, =1 if save requested, =2 if saved.)
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER  FILE*(*)
       INTEGER    MSAVE, STATUS
       LOGICAL    INTER
C
       CHARACTER  STRING*64, FNAME*64, LFILE*24
       INTEGER    LF, IFILE, SAVE_FLAG
       INTEGER    I1, I2, ILIST
C
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/post/include/control_tables.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(ILIST)
C
C  Check current save status
C
       IF (MSAVE.EQ.1) THEN
         WRITE(ILIST,*)'this file is already on the save list'
       ELSEIF (MSAVE.EQ.2) THEN
         STRING='this file has already been saved, '//
     :                        'do you want to save it again? '
         IF (INTER) THEN
           IF (io_yesno(STRING,' ',STATUS)) MSAVE=0
         ELSE
           WRITE(ILIST,*)STRING(1:32)
         ENDIF
       ELSE
         MSAVE=0
       ENDIF
C
C    Find the appropriate save list file
C
       IF (MSAVE.EQ.0) THEN
         CALL io_namfil(FILE,FNAME,0,STATUS)
         IF (STATUS.EQ.0) THEN
           I2=INDEX(FNAME,')')
           I1=INDEX(FNAME(1:I2),':')+1
           LFILE='('//FNAME(I1:I2)//'SAVE-SAMP:LIST'
C
C    Add the filename to the list file
C
           CALL io_nxtlun(IFILE,STATUS)
           OPEN (IFILE, FILE=LFILE, ACCESS='WA', STATUS='UNKNOWN',
     :                                              IOSTAT=STATUS)
           IF (STATUS.EQ.0) THEN
             WRITE(IFILE,*)FILE
             CLOSE(IFILE)
             WRITE(ILIST,*)'... filename added to ',LFILE(1:chr_lenb(LFILE))
C
C    Mark the control tables saved
C
             OPEN (IFILE, FILE=FILE, ACCESS='WX', STATUS='OLD',
     :                                              IOSTAT=STATUS)
             IF (STATUS.EQ.0) THEN
               MSAVE=1
               SAVE_FLAG=2
               CALL SET_SAVE_FLAG(IFILE,SAVE_FLAG,STATUS)
               CALL WRITE_CT(IFILE,STATUS)
               CLOSE(IFILE)
               CT_LUN = 0
             ENDIF
           ENDIF
C
         ENDIF
       ENDIF
C
       END
