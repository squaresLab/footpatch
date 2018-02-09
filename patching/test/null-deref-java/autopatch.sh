#!/bin/bash

source ../../../CONFIG

ERRORS="null_exn_deref_with_type"
FILENAME="Example.java"
PATCH_LOG="AUTOPATCH_LOG.txt"

rm -f $PATCH_LOG

read -p "Autopatching for ${ERRORS} for file ${FILENAME} ok? (y/n)" choice
case "$choice" in
  n|N ) exit 0;;
  * ) echo "OK!";;
esac

printf "[+] Running Infer to find bugs and candidate patches\n" | tee -a $PATCH_LOG
$INFER/infer/bin/infer -g -- javac ${FILENAME}
CMD_RES=$?
printf "[+] Exit status: ${CMD_RES}\n" | tee -a $PATCH_LOG

if [ $CMD_RES -eq 0 ]; then
  printf "[+] Success, continuing... ${p}\n" | tee -a $PATCH_LOG
else
  printf "[-] Infer failed to run\n" | tee -a $PATCH_LOG
  exit 1
fi

# since infer deletes infer-out, we need to keep the initial patch generation 
# version. assume it is called infer-out-patches
printf "[+] Copying infer-out\n" | tee -a $PATCH_LOG

INFER_OUT_PATCHES="infer-out-patches"
PATCHES_PATH="./${INFER_OUT_PATCHES}/footpatch/"

cp -r infer-out $INFER_OUT_PATCHES

printf "[+] Initiate AUTOPATCH!\n" | tee -a $PATCH_LOG

for e in $ERRORS; do
  for p in $(ls ${PATCHES_PATH}/$e/"patches/"*); do
    printf "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n" | tee -a $PATCH_LOG
    printf "[+] Using patch: ${p}\n" | tee -a $PATCH_LOG
    OUT=`patch -b -p0 -d / < $p` # creates a backup .orig file
    printf "Patch output: ${OUT}\n"
    FILE_PATCHED=`echo $OUT | awk '{print $3}'`

    printf "\t[+] Patched: ${FILE_PATCHED}\n" | tee -a $PATCH_LOG

    printf "\t[+] Running Infer...\n" | tee -a $PATCH_LOG
    $INFER/infer/bin/infer -g -- javac ${FILENAME} &> /dev/null
    CMD_RES=$?
    printf "\t[+] Exit status: ${CMD_RES}\n" | tee -a $PATCH_LOG

    if [ $CMD_RES -eq 0 ]; then
      printf "\t[+] Success: ${p}\n" | tee -a $PATCH_LOG
      printf "\t[+] Checking if we destroyed a bug...\n" | tee -a $PATCH_LOG

      # Initially, thought about checking if the fixed version has less issues
      # or smaller bugs.txt file. But if it allows analysis to continue and
      # finds more bugs, this won't work. So simple diff is a good enough
      # signal (something changed in bug output)

      DIFF_RESULTS=$(mktemp)-$(date +"%Y-%m-%d-%H-%M-%S")-diff.txt
      diff infer-out/bugs.txt infer-out-patches/bugs.txt &> $DIFF_RESULTS

      if [ $? -eq 1 ]; then
        printf "\t\t[!] Oh goody, looks like yes!\n" | tee -a $PATCH_LOG
        printf "\t\t[!] Refer to file ${DIFF_RESULTS} for bugs difference\n" | tee -a $PATCH_LOG
        printf "Patch name: ${p}\n" >> $DIFF_RESULTS
     else
        printf "\t\t[?] Looks like no...\n" | tee -a $PATCH_LOG
        rm $DIFF_RESULTS
      fi

      PATCHED_FILE_BACKUP=$(mktemp)-$(date +"%Y-%m-%d-%H-%M-%S")-patched
      printf "\t[+] Copying patched file to ${PATCHED_FILE_BACKUP}\n" | tee -a $PATCH_LOG
      cp ${FILE_PATCHED} $PATCHED_FILE_BACKUP

    elif [ $CMD_RES -eq 1 ]; then
      printf "\t[-] Failed: ${p}\n" | tee -a $PATCH_LOG
    elif [ $CMD_RES -eq 127 ]; then
      printf "\t[-] WTF: ${p}\n" | tee -a $PATCH_LOG
    else
      printf "\t[+] Dont know error: ${CMD_RES}\n" | tee -a $PATCH_LOG
    fi

    printf "\t[+] Restoring file ${FILE_PATCHED}\n" | tee -a $PATCH_LOG
    mv ${FILE_PATCHED}.orig ${FILE_PATCHED}
  done
done
