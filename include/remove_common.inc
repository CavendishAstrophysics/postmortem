C     ******************************************************************
C
C     Remove runtime definition.
C
C     This is a common block version of the remove definition record.
C
C     ******************************************************************

      include '/mrao/post/include/remove_record.inc'

C     Current remove number
      integer         rem_number

      common   / remove_rec / remove_record, rem_number
