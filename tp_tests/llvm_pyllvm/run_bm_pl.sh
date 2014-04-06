echo "running bayes..."
time bash llvm_bm.sh bayes.bc
echo "running kmeans..."
time bash llvm_bm.sh kmeans.bc
echo "running linreg..."
time bash llvm_bm.sh linreg.bc
echo "running logreg..."
time bash llvm_bm.sh logreg.bc
