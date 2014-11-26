C
C Calibration definition runtime common block declaration
C -------------------------------------------------------

      include '/mrao/post/include/cal_record.inc'

C     Current calibration number
      integer         cal_number

      common  / cal_record / cal_record, cal_number
