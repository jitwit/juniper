NB. J profile
NB. JFE sets BINPATH_z_ and ARGV_z_
jpathsep_z_=: '/'&(('\' I.@:= ])})
home=. 2!:5'HOME'
bin =. BINPATH_z_=: 'emacs'
install=. home,'/.guix-profile/share/j'
'addons system tools'=. install&, &.> '/addons';'/system';'/tools'
user=. home,userx=. '/j902-user'
'break config snap temp'=. user&, &.> '/break';'/config';'/snap';'/temp'
ids=. ;:'addons bin break config home install snap system tools temp user'
SystemFolders_j_=: ids,.jpathsep@".&.>ids
md=. 3 : 0
a=. jpathsep y,'/'
if. -.#1!:0 }:a do. for_n. I. a='/' do. 1!:5 :: [ <n{.a end. end.
)
md &.> (user,'/projects');break;config;snap;temp
NB. boot up J and load startup.ijs if it exists
0!:0 <jpathsep (4!:55 (;:'userx ids md'), ids)]system,'/util/boot.ijs'
