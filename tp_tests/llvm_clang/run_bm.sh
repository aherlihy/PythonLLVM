echo "running bayes..."
time bash llvm_bm.sh cpp_bayes.bc
echo "running kmeans..."
time bash llvm_bm.sh cpp_kmeans.bc
echo "running linreg..."
time bash llvm_bm.sh cpp_linreg.bc
echo "running logreg..."
time bash llvm_bm.sh cpp_logreg.bc
