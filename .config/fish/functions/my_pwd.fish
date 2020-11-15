function my_pwd
  echo  (cat ~/pwd) | sed -r 's/\/home\/jeyj0/~/' | sed -r 's/~\/projects\//p:/'
end

