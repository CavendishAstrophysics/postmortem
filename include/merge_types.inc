C         Number of spacing merge types
              integer             num_merge
              parameter         ( num_merge = 10 )

C         Valid merge types.
              character*(80)      merge_types(num_merge)

              integer             no_merge,
     *                            aerial_merge,
     *                            west_aerials, east_aerials,
     *                            hut_sw_merge, hut_merge,
     *                            total_merge, subband_merge,
     *                            channel_merge, fr_aerial_merge
              parameter         ( no_merge        = 1 )
              parameter         ( aerial_merge    = 2 )
              parameter         ( east_aerials    = 3 )
              parameter         ( west_aerials    = 4 )
              parameter         ( fr_aerial_merge = 5 )
              parameter         ( channel_merge   = 6 )
              parameter         ( subband_merge   = 7 )
              parameter         ( hut_sw_merge    = 8 )
              parameter         ( hut_merge       = 9 )
              parameter         ( total_merge     = 10 )

              data    merge_types/
     *  'none ............ no merging, use individual visibilities',
     *  'aerials ......... merge visibilities by aerial',
     *  'east-aerials .... merge by aerials, output east aerials only',
     *  'west-aerials .... merge by aerials, output west aerials only',
     *  'logical-aerials . merge by aerial, for each sub-band/channel (R
     *YLE)',
     *  'channel-merge ... merge frequency channels within sub-bands (RY
     *LE)',
     *  'sub-band-merge .. merge sub-bands within each spacing (RYLE)',
     *  'switch-huts ..... merge switching east and west huts (CLFST)',
     *  'hut-merge ....... merge visibilities by hut (CLFST)',
     *  'total-merge ..... merge all visibilities'/
