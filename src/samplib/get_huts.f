

C+GET_HUTS

       subroutine get_huts ( lun, prompt, default, list, ilist,
     :                                              mhut, nhut, status )
C
C  Reads a hut list from the command line.
C
C  Given:
C      LUN       integer     sample file unit number
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C      MHUT      integer     maximum number of huts in the list
C
C  Returned:
C      LIST      char*(*)    string containing input hut list
C      ILIST     integer(*)  array containing hut index numbers
C      NHUT      integer     number of huts in the list
C      STATUS    integer     status value
C
C  Routine to read a list of huts (aerial groups) from the command line,
C  and return an array containing the index numbers of the huts included,
C  ordered from East to West (increasing order).  The valid formats are:
C
C      -  'HUTS A,C,...'         specifies huts by alphabetic identifier
C      -  'ALL'                  specifies all huts
C
C  The status value should be zero on entry.  The returned status value is
C  zero, unless bad syntax or a user break has been detected.
C
*-
       CHARACTER*(*)  PROMPT, DEFAULT, LIST
       INTEGER  LUN, MHUT, ILIST(MHUT), NHUT, STATUS
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
           CALL SET_HUTS(LUN,LIST(1:LENGTH),ILIST,MHUT,NHUT,STATUS)
           CALL io_enqbch(BATCH)
           IF (STATUS.NE.0) THEN
             CALL io_wrout('*** illegal hut list')
             IF (.NOT.BATCH) STATUS=0
             GOTO 1
           ENDIF
           CALL io_setcli(' ')
         ELSE
           NHUT=0
         ENDIF
       ENDIF
C
       END
