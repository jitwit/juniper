NB. J profile
NB. JFE sets BINPATH_z_ and ARGV_z_

jpathsep_z_=: ]
home=. 2!:5'HOME'
bin =. BINPATH_z_=: 'emacs'

install=. home,'/.guix-profile/share/j'
addons=. install,'/addons'
system=. install,'/system'
tools=. install,'/tools'
userx=. '/j902-user'
user=. home,userx
break=. user,'/break'
config=. user,'/config'
snap=. user,'/snap'
temp=. user,'/temp'
ids=. ;:'addons bin break config home install snap system tools temp user'

SystemFolders_j_=: ids,.jpathsep@".&.>ids

NB. used to create mutable j user directories for temp
NB. files/configuring jqt/projects and so on
md=. 3 : 0 NB. recursive makedir
a=. jpathsep y,'/'
if. -.#1!:0 }:a do.
  for_n. I. a='/' do. 1!:5 :: [ <n{.a end.
end.
)

NB. try to ensure user folders exist
md user,'/projects'
md break
md config
md snap
md temp

NB. boot up J and load startup.ijs if it exists
0!:0 <jpathsep (4!:55 (;:'userx ids md'), ids)]system,'/util/boot.ijs'
require 'viewmat plot'
NB. VISIBLE_jviewmat_ =: 0 NB. todo: add hook to display viewmat in emacs buffer

