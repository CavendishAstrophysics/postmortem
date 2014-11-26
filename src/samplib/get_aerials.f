


C$(5)  Routines for handling spacing lists.

C+GET_AERIALS

       subroutine get_aerials ( lun, prompt, default, list, ilist,
     :                                                mae, nae, status )
C
C  Reads an aerial list from the command line.
C
C  Given:
C      LUN       integer     sample file unit number
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C      MAE       integer     maximum number of aerials in the list
C
C  Returned:
C      LIST      char*(*)    string containing input aerial list
C      ILIST     integer(*)  array containing aerial index numbers
C      NAE       integer     number of aerials in the list
C      STATUS    integer     status value
C
C  Routine to read a list of aerials from the command line, and return
C  an array containing the index numbers of the aerials included, ordered
C  from East to West (increasing order).  Various formats are recognised:
C
C      -  'AE 1,3,5-12,14,...'   specifies aerials by index number
C      -  'HUTS A,C,...'         specifies aerials by hut identifier
C      -  'ALL'                  specifies all aerials
C
C  The status value should be zero on entry.  The returned status value is
C  zero, unless bad syntax or a user break has been detected.
C
*-
       CHARACTER*(*)  PROMPT, DEFAULT, LIST
       INTEGER  LUN, MAE, ILIST(MAE), NAE, STATUS
       INTEGER  LENGTH
       LOGICAL  BATCH
C
    1  IF (STATUS.EQ.0) THEN
C
C    Read in new line of input
C
         CALL io_getcli(PROMPT,DEFAULT,LIST,LENGTH,STATUS)
C
C    Check for validity, resolve the list into index numbers
C
         IF (LENGTH.GT.0) THEN
           CALL SET_AERIALS(LUN,LIST(1:LENGTH),ILIST,MAE,NAE,STATUS)
           CALL io_enqbch(BATCH)
           IF (STATUS.NE.0) THEN
             CALL io_wrout('*** illegal aerial list')
             IF (.NOT.BATCH) STATUS=0
             GOTO 1
           ENDIF
           CALL io_setcli(' ')
         ELSE
           NAE=0
         ENDIF
       ENDIF
C
       END
