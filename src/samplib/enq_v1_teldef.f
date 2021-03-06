


C$(6a)  Control tables version 1 enquiry support routines.

C+ ENQ_V1_TELDEF

       subroutine enq_v1_teldef ( nae_max, nsp_max, nba_max, nch_max, s)
C
C returns the definition of the telescope
C
C Returned
C   NAE_MAX       -     I4     -   max. number of aerial
C   NSP_MAX       -     I4     -   max. number of spacings
C   NBA_MAX       -     I4     -   max. number of sub-bands
C   NCH_MAX       -     I4     -   max. number of channel
C   S             -     I4     -   error return code
C
C CT Version 1 support for ENQ_TELDEF
C
C [PA, 1/11/88]
*-

       integer     s
       integer     nae_max, nsp_max, nba_max, nch_max

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       nae_max = max_aes
       nsp_max = max_spac
       nba_max = 1
       nch_max = 1

       return

       end
