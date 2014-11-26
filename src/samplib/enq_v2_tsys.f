

C+ENQ_V2_TSYS

      subroutine enq_v2_tsys( temps, nominal, s )

C     Version 2 enquire system temperatures

C     Returns
C         Table of system temperatures by aerial
              real    temps(1)
C         Nominal rain gauge value
              real    nominal
C         Status
              integer s

      include    '/mrao/post/include/control_tables.inc'
      include    '/mrao/post/include/control_tab_v2.inc'

      integer i

      do 100, i = 1,max_aes
         temps(i) = Tsys(i)
 100  continue
      nominal = Vrain0

      return

      end
