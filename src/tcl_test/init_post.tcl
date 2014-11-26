#
# System wide init_post :::: initialisation file for Postmortem
#
# In this file we define commands not regarded as part of the base
# postmortem system, but additions, aliases etc.
#

# define news and help files
set Post(newsfile) /mrao/help/post/news_file.news
set Post(helpfile) /mrao/help/post/post_post.help

# move to user specific directory
cd ~/mrao

# setup map directory environment and sample file environment
set env(MAPSDIR) $env(HOME)/images
set env(SAMPDIR) /mraosc/data/ryle

# setup environment for spawned processes (implementation dependent)
set env(PRINTER) hplaser

# Setup welcome message to Postmortem
set Post(Message) \
"
\t Welcome to Test Version $Post(Version) See Help/News for details

\t =>\t See News about experimental version Version $Post(Version)
\t =>\t Re-inititialisation problems believed solved
"

# output message if not running under X
if {!$Xpost} then {
   puts $Post(Message)
}

# run user initialisation file if present
if [file exists $Post(UserDir)/init_post.tcl] then {
  source $Post(UserDir)/init_post.tcl
}
