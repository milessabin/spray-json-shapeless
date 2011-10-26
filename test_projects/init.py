import os


PROJECTS = [
    { 'name' : 'sbt-deps',
      'url' : 'https://github.com/hdeshev/sbt-deps.git',
      'sbt_cmd' : 'sbt7',
      'config' : """
                 (
                 :use-sbt t
                 :sbt-subprojects (
                   (:name "web")
                   (:name "core")
                   )
                 )
                 """
      },

    { 'name' : 'anti-xml',
      'url' : 'https://github.com/djspiewak/anti-xml.git',
      'sbt_cmd' : 'sbt11',
      'config' : """
                 (
                 :use-sbt t
                 )
                 """
      },

    { 'name' : 'rogue',
      'url' : 'https://github.com/foursquare/rogue.git',
      'sbt_cmd' : 'sbt10',
      'config' : """
                 (
                 :use-sbt t
                 :sbt-version "0.10.0"
                 )
                 """
      }
    ]


for proj in PROJECTS:
    nm = proj['name']
    os.system("rm -rf " + nm)
    os.system("git clone " + proj['url'])
    os.chdir(nm)
    os.system(proj['sbt_cmd'] + " update")
    f = open(".ensime", 'w')
    f.write(proj['config'])
    f.close()
    os.chdir("../")
