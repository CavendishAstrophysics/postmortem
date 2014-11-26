

C+ ENQ_V2_OBSDEF

       subroutine enq_v2_obsdef ( nae_o, nsp_o, nba_o, nch_o, s )
C
C returns the definition of the current observation
C
C Returned
C   NAE_O         -     I4     -   actual number of aerial
C   NSP_O         -     I4     -   actual number of spacings
C   NBA_O         -     I4     -   actual number of sub-bands
C   NCH_O         -     I4     -   actual number of channel
C   S             -     I4     -   error return
C
C CT Version 2 support for ENQ_OBSDEF
C
C [PA, 1/11/88]
*-

       integer     s
       integer     nae_o, nsp_o, nba_o, nch_o

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

       nae_o = naes
       nsp_o = nsp
       nba_o = nsubb
       nch_o = nchannel

       return

       end
