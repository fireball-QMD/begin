# this script is a free (GPLv3) Copyright (C) 2013 by Daniel González Trabada
# This program comes with ABSOLUTELY NO WARRANTY; 


#./begin.sh -ex 9 -z 001 -ele H -mass 1.0079 -orb s -excited 3 1.00 0.95 -n 1.00 -n0 1.00 -r 4.00
#./begin.sh -ex 9 -z 001 -ele H -mass 1.0079 -orb s -excited 4 1.00 0.95 -n 1.00 -n0 1.00 -r 4.00
#./begin.sh -ex 9 -z 006 -ele C -mass 12.011 -orb s -n 2.00 -n0 1.00 -r 4.00 -orb p -n 0.80 -n0 3.00 -r 4.50
#./begin.sh -ex 9 -z 007 -ele N -mass 14.0067 -orb s -n 2.00 -n0 2.00 -r 4.00 -orb p -n 2.00 -n0 3.00 -r 4.50
#./begin.sh -ex 9 -z 008 -ele O -mass 15.9994 -orb s -n 2.00 -n0 2.00 -r 3.70 -orb p -n 4.00 -n0 4.00 -r 4.20
#./begin.sh -ex 9 -z 001 -ele H -mass 1.0079 -orb s -excited 3 1.00 0.85 -n 1.00 -n0 1.00 -r 4.00
#  -ex 9 -z 006 -ele C -mass 12.011 -orb s -n 1.40 -n0 1.00 -r 4.00 -orb p -n 1.30 -n0 3.00 -r 4.50
#  -ex 9 -z 006 -ele C -mass 12.011 -orb s -n 1.40 -n0 1.00 -r 4.00 -orb p -n 1.30 -n0 3.00 -r 4.50 -orb d -n 0.00 -n0 0.00 -r 4.50 -pot -vr 1 -v0 500
#  -ex 9 -z 006 -ele C -mass 12.011 -orb s -n 1.40 -n0 1.00 -r 4.00 -orb p -n 1.30 -n0 3.00 -tol 0.01 -orb d -n 0.00 -n0 0.00 -r 4.50 
#  -ex 9 -z 006 -ele C -mass 12.011 -orb s -n 1.40 -n0 1.00 -r 4.00 -orb p -n 1.30 -n0 3.00 -r 4.50 -orb d -n 0.00 -n0 0.00 -r 4.50 -ion 0.50 0.00 0.00 
#  -ex 9 -z 006 -ele C -mass 12.011 -excited 1 -orb s -n 1.40 -n0 1.00 -r 4.00 -orb p -n 1.30 -n0 3.00 -r 4.50 -orb d -n 0.00 -n0 0.00 -r 4.50 -ion 0.50 0.00 0.00 
# -z 001 -pp 1.915 -ex 9 -ele H -mass 1.0079 -orb s -n 3.12 -n0 1 -r 4.00 


ex=9
o=0
pp=FALSE
excited=N
here=$(pwd)
for i in $@
do
if [[ $i == '-z' ]] ; then  z=$2 ; shift 2 ; fi
if [[ $i == '-ex' ]] ; then  ex=$2 ; shift 2 ; fi
if [[ $i == '-ele' ]] ; then  ele=$2 ; shift 2 ; fi
if [[ $i == '-mass' ]] ; then  mass=$2 ; shift 2 ; fi
if [[ $i == '-excited' ]] ; then excited=Y;  opEXC=$2 ;  shift 2 ; 
  if [[ $opEXC == 3 || $opEXC == 4 ]] ; then 
     if [[ $o > 0 ]] ; then ns_mix=$1 ; mixs=$2 ; shift 2; fi  
     if [[ $o > 1 ]] ; then np_mix=$1 ; mixp=$2 ; shift 2;  fi  
     if [[ $o > 2 ]] ; then nd_mix=$1 ; mixd=$2 ; shift 2;  fi
  fi
 fi
# metemos dos parametros mas PP Z_pp ! no poner antes de leer Z
if [[ $i == '-pp' ]]
then
echo hacemos PP $i
Zpp_new=$2
shift 2
pp=TRUE
cp ${z}.pp ${z}.pp.back
Zpp=$(grep Z\ val ${z}.pp | cut -d'!' -f1)
#el valor de Z esta en la linea 18
head -17 $z.pp > aux
echo '   '$Zpp_new'                     ! Z val' >> aux
tail -$(($(wc -l  ${z}.pp | cut -d' ' -f1)-18)) ${z}.pp >>  aux
cp aux ${z}.pp
fi

 
if [[ $i == '-orb' ]]  
 then  
 o=$((o+1));
 orb[$o]=$2; 
 ion[$o]=0; tol[$o]=FALSE; 
 auto[$o]=FALSE; pot[$o]=FALSE; 
 v0[$o]=0.00; vr[$o]=0.00; shift 2 ;
 if [[ ${orb[o]} == 's' ]] ; then  l[$o]=0; fi
 if [[ ${orb[o]} == 'p' ]] ; then  l[$o]=1; fi
 if [[ ${orb[o]} == 'd' ]] ; then  l[$o]=2; fi
 fi
if [[ $i == '-n' ]] ; then n[$o]=$2 ; shift 2 ; fi
if [[ $i == '-ion' ]] ; then ion[$o]=$o; ions[$o]=$2 ; ionp[$o]=$3 ; iond[$o]=$4 ; shift 4 ; fi
if [[ $i == '-n0' ]] ; then n0[$o]=$2 ; shift 2 ; fi
if [[ $i == '-r' ]] ; then r[$o]=$2 ; shift 2 ; fi
if [[ $i == '-tol' ]] ; then auto[$o]=TRUE; tol[$o]=$2; r[$o]=14.00 ; shift 2 ; fi
if [[ $i == '-pot' ]] ; then pot[$o]=TRUE ; shift 1 ; fi
if [[ $i == '-vr' ]] ; then vr[$o]=$2 ; shift 2 ; fi
if [[ $i == '-v0' ]] ; then v0[$o]=$2 ; shift 2 ; fi

done




# El ion solo esta pensado para spd sin potencial confinante ni tolerancias
for((i=1;i<=$o;i++))
do
auxexcited=N
echo ion $i = ${ion[i]}
if [[ ${ion[i]} != 0 ]] ; then
a="-z $z -ele $ele -mass $mass -ex $ex"
k=${ion[i]}
j=1
a=$a" -orb ${orb[j]} -n ${ions[k]} -n0 ${n0[j]} -r ${r[j]}"
j=2
a=$a" -orb ${orb[j]} -n ${ionp[k]} -n0 ${n0[j]} -r ${r[j]}"
j=3
a=$a" -orb ${orb[j]} -n ${iond[k]} -n0 ${n0[j]} -r ${r[j]}"
 echo $a >> log 
./${0} $a
 rm -fr cinput.ion
 mv cinput cinput.ion
fi
done


function initial {
if [[ -f initial.x ]]
then echo run
else make ;make clean ;
fi
echo ::: initial.x ::: $Z ${orb[1]} ${orb[2]} ${orb[3]} ${n[1]} ${n[2]} ${n[3]} ${r[1]} ${r[2]} ${r[3]}

#confinementP=${v0[1]}$'\n'${vr[1]}$'\n'${v0[1]}$'\n'${vr[1]}

if [[ $o == 1 ]]
then
if [[ $excited == Y ]]; then
auxexcited=Y$'\n'1
if [[ $opEXC == 2 ]] ; then
 # auxexcited=Y$'\n'2$'\n'${n[1]}
 auxexcited=Y$'\n'2$'\n'$(echo ${n[1]}-0 | bc -l)
 #ns=1 Z=1 y nse=1 con Z++=3
 fi
 if [[ $opEXC == 3 ||  $opEXC == 4 ]];  then
 auxexcited=Y$'\n'3$'\n'$ns_mix$'\n'$mixs
 fi
fi
P=N
if [[ ${pot[o]} == 'TRUE' ]]; then
 P=Y$'\n'${v0[1]}$'\n'${vr[1]}$'\n'${v0[1]}$'\n'${vr[1]}
fi
cat << EOF|./initial.x > /dev/null
$z
N
${orb[1]}
c
N
${n[1]} 
N
${r[1]}
${auxexcited}
N
${ex}
$P
EOF
fi


if [[ $o == 2 ]]
then
if [[ $excited == Y ]]; then
 auxexcited=Y$'\n'1
 if [[ $opEXC == 2 ]]
 then
  auxexcited=Y$'\n'2$'\n'$(echo ${n[1]}-1 | bc -l)$'\n'$(echo ${n[2]}-1 | bc -l)
 fi
 if [[ $opEXC == 3 ||  $opEXC == 4 ]];  then
  auxexcited=Y$'\n'3$'\n'$ns_mix$'\n'$np_mix$'\n'$mixs$'\n'$mixp
 fi
fi

P=N
if [[ ${pot[o]} == 'TRUE' ]]; then
 P=Y$'\n'${v0[1]}$'\n'${vr[1]}$'\n'${v0[2]}$'\n'${vr[2]}$'\n'${v0[1]}$'\n'${vr[1]}$'\n'${v0[2]}$'\n'${vr[2]}
fi

cat << EOF|./initial.x > /dev/null
$z
N
${orb[1]}
${orb[2]}
c
N
${n[1]} 
${n[2]}
N
${r[1]}
${r[2]}
${auxexcited}
N
${ex}
$P
EOF
fi

if [[ $o == 3 ]]
then
if [[ $excited == Y ]]; then
 auxexcited=Y$'\n'1
 if [[ $opEXC == 2 ]]
 then
  auxexcited=Y$'\n'2$'\n'$(echo ${n[1]}-1 | bc -l)$'\n'$(echo ${n[2]}-1 | bc -l)$'\n'$(echo ${n[3]}-1 | bc -l)
 fi
 if [[ $opEXC == 3 ||  $opEXC == 4 ]];  then
  auxexcited=Y$'\n'3$'\n'$ns_mix$'\n'$np_mix$'\n'$nd_mix$'\n'$mixs$'\n'$mixp$'\n'$mixd
 fi
fi
P=N
if [[ ${pot[o]} == 'TRUE' ]]; then
 P=Y$'\n'${v0[1]}$'\n'${vr[1]}$'\n'${v0[2]}$'\n'${vr[2]}$'\n'${v0[3]}$'\n'${vr[3]}$'\n'${v0[1]}$'\n'${vr[1]}$'\n'${v0[2]}$'\n'${vr[2]}$'\n'${v0[3]}$'\n'${vr[3]}
fi

cat << EOF|./initial.x > /dev/null
$z
N
${orb[1]}
${orb[2]}
${orb[3]}
c
N
${n[1]} 
${n[2]}
${n[3]}
N
${r[1]}
${r[2]}
${r[3]}
${auxexcited}
N
${ex}
$P
EOF
fi

echo excited :: ${n[1]} ${n[2]} ${n[3]} ::  $excited
make 
./begin.x 
make clean
for((i=1;i<=$o;i++))
do
 aux_r[i]=$(echo ${r[i]} | sed 's/\.//g')
 aux_r[i]=${aux_r[i]::3}
done
}



function cambiar_carga {
head -3  $archivo > aux.temp
echo '     '$( head -4 $archivo| tail -1 | cut -c-20)' '$carga  >> aux.temp
tail -$(($(wc -l $archivo | cut -d' ' -f1)-4)) $archivo >> aux.temp
mv aux.temp $archivo
}

function Xinput {
cat << EOF > $ele.input
$ele 
$z
$mass  
cinput/${z}.pp
cinput/$na0
EOF

if [[ $excited == Y &&  $opEXC != 3  ]]
then
echo $((o*2)) >> $ele.input
else
echo $o >> $ele.input
fi 

for((i=1;i<=$o;i++))
do
echo ${l[i]} >>  $ele.input
echo ${n0[i]}  >>  $ele.input
echo ${r[i]}  >>  $ele.input
echo cinput/${z}_${aux_r[i]}.wf$i  >>  $ele.input
echo cinput/${z}_${aux_r[i]}.na$i  >>  $ele.input
done

if [[ $excited == Y && $opEXC != 3 ]]
then
for((i=1;i<=$o;i++))
do
echo ${l[i]} >>  $ele.input
echo 0  >>  $ele.input
echo ${r[i]}  >>  $ele.input
echo cinput/${z}_${aux_r[i]}.ewf$i  >>  $ele.input
echo cinput/${z}_${aux_r[i]}.ena$i  >>  $ele.input
done
fi
echo 1 >> $ele.input
echo 0.5 >>  $ele.input 
echo '0.25 0.25 0.25 0.25 0.25 0.25' >>  $ele.input
}

function corte {
rcorte=$(head -4 $archivo | tail -1 | tr -s ' ' | cut -d' ' -f2)
nlineas=$(wc -l $archivo | cut -d' ' -f1)
cat $archivo | tail -n$(($nlineas-5)) | sed s/D/E/g > aux.temp
echo $(python -c "
x0 = []
for line in file(\"aux.temp\"):
   line = line.split()
   x = line[0]
   x0.append(float(x))
j=0
for i in range(len(x0)):
   if x0[i]> $TOL:
    j=i
r=$rcorte*j/($nlineas-5)
print '%6.1f0' % (r)")
rm -fr aux.temp
}

#--------------------RUN------------------
#-----------------------------------------

rm -fr cinput
mkdir cinput
cp ${z}*.pp cinput 
echo "ejecutamos initial..."
initial
echo "... fin de initial"
echo -----------------------------------------------------------------------------------------------------

#---- reajustamos radio de corte -------
redo=0
for((i=1;i<=$o;i++))
do
 if [[ ${auto[i]} == TRUE ]]
 then
  archivo=${z}_${aux_r[i]}.wf$i
  echo $archivo $i ${tol[i]}
  TOL=${tol[i]}
  r[$i]=$(corte)
  redo=1
 fi
done
#----------------------------------------
if [[ $redo == 1 ]]
then
 initial
fi

for((i=1;i<=$o;i++))
do
 archivo=${z}_${aux_r[i]}.wf$i  
 carga=${n0[i]}
 cambiar_carga 
 mv $archivo  cinput/
done

#los excited son con carga 0
if [[ $excited == Y && $opEXC != 3 ]]
then
for((i=1;i<=$o;i++))
do
 archivo=${z}_${aux_r[i]}.ewf$i  
 mv $archivo  cinput/
done
fi



for((i=1;i<=$o;i++))
do
if [[ ${ion[i]} != 0 ]]
then
 ION=${ion[i]}
 cp cinput.ion/${z}_${aux_r[ION]}.wf$ION cinput/
# echo cp cinput.ion/${z}_${aux_r[ION]}.wf$ION cinput/ >> log
fi
done

cd begin_vnn/
mv ../cinput/* .

if [[ $pp = TRUE ]]
then
cp ../${z}.pp.back ${z}.pp
fi

if [[  $opEXC == 4 ]] 
then
opEXC=2
fi

initial

na0=$(ls *na0 | cut -d' ' -f1)
mv *na* ../cinput/
mv *wf* ../cinput/
mv *pp ../cinput/

cd ../cinput

Xinput

#cp ../${z}**p .

rm -fr fort.100


if [[ $pp = TRUE ]]
then
cd $here
cp ${z}.pp.back ${z}.pp
fi

