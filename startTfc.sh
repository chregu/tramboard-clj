cd /home/chregu/tfc
kill $(pgrep -U chregu java)
nohup java -Dfile.encoding=UTF-8 -jar tramboard-clj.jar &
