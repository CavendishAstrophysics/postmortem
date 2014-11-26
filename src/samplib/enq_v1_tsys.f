

C+ENQ_V1_TSYS

      subroutine enq_v1_tsys( temps, nominal, s )

C     Version 1 enquire system temperatures

C     Returns
C         Table of system temperatures by aerial
              real    temps(1)
C         Nominal rain gauge value
              real    nominal
C         Status
              integer s

      include    '/mrao/post/include/control_tables.inc'
      include    '/mrao/post/include/control_tab_v1.inc'

      integer i

      do 100, i = 1,max_aes
         temps(i) = 1
 100  continue
      nominal = 1

      return

      end
