C     /mrao/post/include/sf_buffer.inc

C     *********************************************************
C
C     Common block containing sample file buffer.
C
C     *********************************************************

C
      integer*4       read_buff_len
      parameter     ( read_buff_len = 2**16 )

      integer*4       buffer(   read_buff_len )
      real*4          rbuff (   read_buff_len )
      integer*2       ibuff ( 2*read_buff_len )
      equivalence   ( buffer, rbuff )
      equivalence   ( buffer, ibuff )
      common        / buffer / buffer

