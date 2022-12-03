test -f Install.log && rm Install.log
header
for pack in $PACKAGES; do
   if test -d $pack; then
		echo "pack=$pack"
		sleep 1
		case $pack in
		mpich-1.2.6) 
			echo "About to call handle_mpich_126"; sleep 1;
			(cd $pack; handle_mpich_126;) 2>&1 | tee -a Install.log
		;;
		pamr) 
			handle_pamr;
			echo "+++++++++++++++++++++++++++++++++++++++" | tee -a Install.log
			echo "Configuring and installing in $pack ..." | tee -a Install.log
			echo "+++++++++++++++++++++++++++++++++++++++" | tee -a Install.log
			(cd $pack; ./configure --prefix=$PREFIX; make install) 2>&1 | tee -a Install.log
		;;
		*)
			echo "+++++++++++++++++++++++++++++++++++++++" | tee -a Install.log
			echo "Configuring and installing in $pack ..." | tee -a Install.log
			echo "+++++++++++++++++++++++++++++++++++++++" | tee -a Install.log
			(cd $pack; ./configure --prefix=$PREFIX; make install) 2>&1 | tee -a Install.log
		;;
		esac
   else
      warn "Directory $pack not found in this distribution." 
   fi
done
