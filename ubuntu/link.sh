/usr/lib/gcc/x86_64-linux-gnu/4.6.1/collect2 \
  --build-id --no-add-needed --as-needed --eh-frame-hdr -m elf_x86_64 \
  --hash-style=gnu -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
  -z relro -o "${1:-arx}" \
  -u ghczmprim_GHCziTypes_Izh_static_info \
  -u ghczmprim_GHCziTypes_Czh_static_info \
  -u ghczmprim_GHCziTypes_Fzh_static_info \
  -u ghczmprim_GHCziTypes_Dzh_static_info \
  -u base_GHCziPtr_Ptr_static_info \
  -u base_GHCziWord_Wzh_static_info \
  -u base_GHCziInt_I8zh_static_info \
  -u base_GHCziInt_I16zh_static_info \
  -u base_GHCziInt_I32zh_static_info \
  -u base_GHCziInt_I64zh_static_info \
  -u base_GHCziWord_W8zh_static_info \
  -u base_GHCziWord_W16zh_static_info \
  -u base_GHCziWord_W32zh_static_info \
  -u base_GHCziWord_W64zh_static_info \
  -u base_GHCziStable_StablePtr_static_info \
  -u ghczmprim_GHCziTypes_Izh_con_info \
  -u ghczmprim_GHCziTypes_Czh_con_info \
  -u ghczmprim_GHCziTypes_Fzh_con_info \
  -u ghczmprim_GHCziTypes_Dzh_con_info \
  -u base_GHCziPtr_Ptr_con_info \
  -u base_GHCziPtr_FunPtr_con_info \
  -u base_GHCziStable_StablePtr_con_info \
  -u ghczmprim_GHCziBool_False_closure \
  -u ghczmprim_GHCziBool_True_closure \
  -u base_GHCziPack_unpackCString_closure \
  -u base_GHCziIOziException_stackOverflow_closure \
  -u base_GHCziIOziException_heapOverflow_closure \
  -u base_ControlziExceptionziBase_nonTermination_closure \
  -u base_GHCziIOziException_blockedIndefinitelyOnMVar_closure \
  -u base_GHCziIOziException_blockedIndefinitelyOnSTM_closure \
  -u base_ControlziExceptionziBase_nestedAtomically_closure \
  -u base_GHCziWeak_runFinalizzerBatch_closure \
  -u base_GHCziTopHandler_runIO_closure \
  -u base_GHCziTopHandler_runNonIO_closure \
  -u base_GHCziConcziIO_ensureIOManagerIsRunning_closure \
  -u base_GHCziConcziSync_runSparks_closure \
  -u base_GHCziConcziSignal_runHandlers_closure \
  /usr/lib/gcc/x86_64-linux-gnu/4.6.1/../../../x86_64-linux-gnu/crt1.o \
  /usr/lib/gcc/x86_64-linux-gnu/4.6.1/../../../x86_64-linux-gnu/crti.o \
  /usr/lib/gcc/x86_64-linux-gnu/4.6.1/crtbegin.o \
  -L/usr/lib/ghc-7.0.3/process-1.0.1.5 \
  -L/usr/local/lib/parsec-3.1.2/ghc-7.0.3 \
  -L/usr/local/lib/mtl-2.0.1.0/ghc-7.0.3 \
  -L/usr/local/lib/transformers-0.2.2.0/ghc-7.0.3 \
  -L/usr/local/lib/attoparsec-0.9.1.2/ghc-7.0.3 \
  -L/usr/local/lib/vector-algorithms-0.5.3/ghc-7.0.3 \
  -L/usr/local/lib/shell-escape-0.1.1/ghc-7.0.3 \
  -L/usr/local/lib/vector-0.9/ghc-7.0.3 \
  -L/usr/local/lib/primitive-0.4.0.1/ghc-7.0.3 \
  -L/usr/local/lib/file-embed-0.0.4.1/ghc-7.0.3 \
  -L/usr/lib/ghc-7.0.3/template-haskell-2.5.0.0 \
  -L/usr/lib/ghc-7.0.3/pretty-1.0.1.2 \
  -L/usr/lib/ghc-7.0.3/directory-1.1.0.0 \
  -L/usr/lib/ghc-7.0.3/unix-2.4.2.0 \
  -L/usr/lib/ghc-7.0.3/old-time-1.0.0.6 \
  -L/usr/lib/ghc-7.0.3/old-locale-1.0.0.2 \
  -L/usr/lib/ghc-7.0.3/filepath-1.2.0.0 \
  -L/usr/local/lib/bytestring-nums-0.3.5/ghc-7.0.3 \
  -L/usr/local/lib/blaze-builder-0.3.0.1/ghc-7.0.3 \
  -L/usr/local/lib/text-0.11.1.5/ghc-7.0.3 \
  -L/usr/local/lib/deepseq-1.2.0.1/ghc-7.0.3 \
  -L/usr/local/lib/binary-0.5.0.2/ghc-7.0.3 \
  -L/usr/lib/ghc-7.0.3/containers-0.4.0.0 \
  -L/usr/local/lib/bytestring-0.9.2.0/ghc-7.0.3 \
  -L/usr/lib/ghc-7.0.3/array-0.3.0.2 \
  -L/usr/lib/ghc-7.0.3/base-4.3.1.0 \
  -L/usr/lib/ghc-7.0.3/integer-gmp-0.2.0.3 \
  -L/usr/lib/ghc-7.0.3/ghc-prim-0.2.0.0 \
  -L/usr/lib/ghc-7.0.3 \
  -L/usr/lib/gcc/x86_64-linux-gnu/4.6.1 \
  -L/usr/lib/gcc/x86_64-linux-gnu/4.6.1/../../../x86_64-linux-gnu \
  -L/usr/lib/gcc/x86_64-linux-gnu/4.6.1/../../../../lib \
  -L/lib/x86_64-linux-gnu \
  -L/lib/../lib \
  -L/usr/lib/x86_64-linux-gnu \
  -L/usr/lib/../lib \
  -L/usr/lib/gcc/x86_64-linux-gnu/4.6.1/../../.. \
  ./tmp/System/Posix/ARX.o \
  ./tmp/System/Posix/ARX/CLI.o \
  ./tmp/Main.o \
  ./tmp/System/Posix/ARX/CLI/CLTokens.o \
  ./tmp/System/Posix/ARX/CLI/Options.o \
  ./tmp/System/Posix/ARX/HEREDat.o \
  ./tmp/System/Posix/ARX/Programs.o \
  ./tmp/System/Posix/ARX/Sh.o \
  ./tmp/System/Posix/ARX/Tar.o \
  ./tmp/System/Posix/ARX/TMPXTools.o \
  ./tmp/System/Posix/ARX/BlazeIsString.o \
  -lHSrtsmain \
  -lHSprocess-1.0.1.5 \
  -lHSparsec-3.1.2 \
  -lHSmtl-2.0.1.0 \
  -lHStransformers-0.2.2.0 \
  -lHSattoparsec-0.9.1.2 \
  -lHSvector-algorithms-0.5.3 \
  -lHSshell-escape-0.1.1 \
  -lHSvector-0.9 \
  -lHSprimitive-0.4.0.1 \
  -lHSfile-embed-0.0.4.1 \
  -lHStemplate-haskell-2.5.0.0 \
  -lHSpretty-1.0.1.2 \
  -lHSdirectory-1.1.0.0 \
  -lHSunix-2.4.2.0 \
  -lrt \
  -lutil \
  -ldl \
  -lpthread \
  -lHSold-time-1.0.0.6 \
  -lHSold-locale-1.0.0.2 \
  -lHSfilepath-1.2.0.0 \
  -lHSbytestring-nums-0.3.5 \
  -lHSblaze-builder-0.3.0.1 \
  -lHStext-0.11.1.5 \
  -lHSdeepseq-1.2.0.1 \
  -lHSbinary-0.5.0.2 \
  -lHScontainers-0.4.0.0 \
  -lHSbytestring-0.9.2.0 \
  -lHSarray-0.3.0.2 \
  -lHSbase-4.3.1.0 \
  -lHSinteger-gmp-0.2.0.3 \
  -lHSghc-prim-0.2.0.0 \
  -lHSrts \
  -lm \
  -lrt \
  -ldl \
  -lgcc --as-needed \
  -lgcc_s --no-as-needed \
  -lc \
  -lgcc --as-needed \
  -lgcc_s --no-as-needed \
  /usr/lib/gcc/x86_64-linux-gnu/4.6.1/crtend.o \
  /usr/lib/gcc/x86_64-linux-gnu/4.6.1/../../../x86_64-linux-gnu/crtn.o \
  -static \
  -lgmp \
  -lffi \

