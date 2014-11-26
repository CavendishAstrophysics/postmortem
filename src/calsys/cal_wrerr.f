


C     ******************************************************************


*+cal_wrerr

       subroutine cal_wrerr (status, text)
C
C  Writes an error message to the error device.
C
C  Given:
C      STATUS    integer     status value
C      TEXT      char*(*)    message text
C
C  Writes an error message to the error device, appropriate to the given
C  status value, and including the message text if this is not blank.
C  The message is also written to the error log file if this has been
C  set up by a previous call to IOLIB routine io_setlog.
C
C  This routine sets up the error message file for the SAMPLIB library
C  and calls the IOLIB routine io_wrerr.
C
*-
       character  text*(*)
       integer    istat, status
C
       character   errfil*36
       parameter ( errfil = '/mrao/post/include/calib_errors.inc')
C
       include '/mrao/include/iolib_errors.inc'
C
       istat=IOLIB_OK
       call io_setmsg(errfil,istat)
       if (istat.ne.IOLIB_OK) call io_wrerr(istat,errfil)
C
       call io_wrerr(status,text)
C
       end
