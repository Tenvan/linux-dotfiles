function next_line ()
  echo
  fish_prompt
  set_color normal
end

function change_to_work ()
  cd $WORK_DIR
end

function kill_node ()
  echo
  echo '##################################'
  echo '# kill all nodejs processes...'
  echo '#'
  echo 

  echo '=> current node processes'
  
  ps -lC node

  echo kill it

  killall 'node'
  killall 'ng '

  echo '== node processes after kill'
  
  ps -lC node
end

function fish_reload_config
  echo Reload config...
  omf reload
end

function start_task_1
  title "Task 1 => Dev Server"
  clear
  echo "Start Task 1"
  change_to_work
  yarn server:dev
end

function start_task_2
  title "Task 2 => Pug Watch"
  clear
  echo "Start Task 2"
  change_to_work
  cd src/client
  yarn pug:watch
end

function start_task_3
  title "Task 3 => Compiler"
  clear
  echo "Start Task 3"
  change_to_work
  cd src/client
  yarn start
end

function start_task_4
  title "Task 4 => Compiler (HMR)"
  clear
  echo "Start Task 4"
  change_to_work
  cd src/client
  yarn start:client:hmr --port 4201
end

function start_task_5
  title "Task 5 => Compiler (AOT)"
  clear
  echo "Start Task 5"
  change_to_work
  cd src/client
  yarn start:client:dev --aot
end
