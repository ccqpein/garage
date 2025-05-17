package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath" // For unique file naming if needed
	"testing"
	"time" // For more unique file naming
)

// --- Configuration ---
// PLEASE UPDATE THESE PATHS BEFORE RUNNING THE BENCHMARK
const (
	// sourceFilePath is the absolute path to your large source file.
	sourceFilePath = "/path/to/source/your_large_file.dat" //
	// targetDir is the absolute path to the directory where copied files will be temporarily stored.
	targetDir = "/path/to/target/" // Ensure this directory exists and is writable
)

// --- End Configuration ---

var (
	globalSourceFileSize int64 // To store the size of the source file
	isSourceInfoLoaded   bool  // Flag to ensure source info is loaded once
	sourceInfoErr        error // To store any error during source info loading
)

// loadSourceInfo retrieves the size of the source file.
// This is called once before the benchmarks run.
func loadSourceInfo() {
	if isSourceInfoLoaded {
		return
	}
	fileInfo, err := os.Stat(sourceFilePath)
	if err != nil {
		sourceInfoErr = fmt.Errorf("failed to stat source file '%s': %w. Please ensure it exists and is accessible", sourceFilePath, err)
		isSourceInfoLoaded = true // Mark as loaded to prevent repeated errors
		return
	}
	if fileInfo.IsDir() {
		sourceInfoErr = fmt.Errorf("source file path '%s' is a directory, not a file", sourceFilePath)
		isSourceInfoLoaded = true
		return
	}
	globalSourceFileSize = fileInfo.Size()
	if globalSourceFileSize == 0 {
		sourceInfoErr = fmt.Errorf("source file '%s' is empty. Please use a file with content", sourceFilePath)
	}
	isSourceInfoLoaded = true
}

// copyFileWithBuffer performs the actual file copy using a specified buffer size.
func copyFileWithBuffer(srcPath, dstPath string, bufferSize int) error {
	sourceFile, err := os.Open(srcPath)
	if err != nil {
		return fmt.Errorf("could not open source file %s: %w", srcPath, err)
	}
	defer sourceFile.Close()

	destinationFile, err := os.Create(dstPath) // Creates or truncates
	if err != nil {
		return fmt.Errorf("could not create destination file %s: %w", dstPath, err)
	}
	defer destinationFile.Close()

	buf := make([]byte, bufferSize)
	_, err = io.CopyBuffer(destinationFile, sourceFile, buf)
	if err != nil {
		// It's good practice to attempt to remove the partially written destination file on error
		os.Remove(dstPath)
		return fmt.Errorf("could not copy data: %w", err)
	}

	err = destinationFile.Sync() // Ensure data is flushed to disk
	if err != nil {
		os.Remove(dstPath) // Also remove if sync fails
		return fmt.Errorf("failed to sync destination file %s: %w", dstPath, err)
	}
	return nil
}

// runBenchmark is a helper to run the copy operation for a given buffer size.
func runBenchmark(b *testing.B, bufferName string, bufferSize int) {
	if !isSourceInfoLoaded { // Should have been called by TestMain, but as a safeguard
		loadSourceInfo()
	}
	if sourceInfoErr != nil {
		b.Fatalf("Benchmark setup failed: %v", sourceInfoErr)
	}
	if globalSourceFileSize == 0 {
		b.Fatalf("Benchmark setup failed: Source file size is 0 or could not be read.")
	}

	b.SetBytes(globalSourceFileSize) // Set bytes processed per operation

	// Construct a unique destination file name for this benchmark run
	// to minimize interference and make cleanup easier.
	// Using bufferName for clarity, and a timestamp/nano for uniqueness if run multiple times quickly.
	dstFileName := fmt.Sprintf("benchmark_copy_%s_%d.tmp", bufferName, time.Now().UnixNano())
	dstPath := filepath.Join(targetDir, dstFileName)

	b.ResetTimer() // Start timing after setup
	for i := 0; i < b.N; i++ {
		// For each iteration, we'll use the same dstPath for this specific benchmark function's b.N loop.
		// Delete before copy to simulate a fresh write.
		// Stop/Start timer around OS operations not directly part of the copy logic.
		b.StopTimer()
		err := os.Remove(dstPath)
		if err != nil && !os.IsNotExist(err) {
			b.Fatalf("Failed to remove previous destination file %s: %v", dstPath, err)
		}
		b.StartTimer()

		err = copyFileWithBuffer(sourceFilePath, dstPath, bufferSize)
		if err != nil {
			b.Fatalf("Copy failed: %v", err)
		}
	}
	b.StopTimer() // Stop timer after all iterations

	// Clean up the last destination file created by this benchmark
	err := os.Remove(dstPath)
	if err != nil && !os.IsNotExist(err) {
		b.Logf("Warning: failed to remove destination file %s after benchmark: %v", dstPath, err)
	}
}

// --- Benchmark Functions ---

func BenchmarkCopyFile_DefaultIoCopy(b *testing.B) {
	if !isSourceInfoLoaded {
		loadSourceInfo()
	}
	if sourceInfoErr != nil {
		b.Fatalf("Benchmark setup failed: %v", sourceInfoErr)
	}
	if globalSourceFileSize == 0 {
		b.Fatalf("Benchmark setup failed: Source file size is 0 or could not be read.")
	}

	b.SetBytes(globalSourceFileSize)
	dstFileName := fmt.Sprintf("benchmark_copy_DefaultIoCopy_%d.tmp", time.Now().UnixNano())
	dstPath := filepath.Join(targetDir, dstFileName)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		err := os.Remove(dstPath)
		if err != nil && !os.IsNotExist(err) {
			b.Fatalf("Failed to remove previous destination file %s: %v", dstPath, err)
		}

		sourceFile, errOpen := os.Open(sourceFilePath)
		if errOpen != nil {
			b.Fatalf("Failed to open source file %s: %v", sourceFilePath, errOpen)
		}
		destinationFile, errCreate := os.Create(dstPath)
		if errCreate != nil {
			sourceFile.Close()
			b.Fatalf("Failed to create destination file %s: %v", dstPath, errCreate)
		}
		b.StartTimer()

		_, errCopy := io.Copy(destinationFile, sourceFile) // Uses internal 32KB buffer

		b.StopTimer() // Stop before closes and potential errors
		if sourceFile != nil {
			sourceFile.Close()
		}
		if destinationFile != nil {
			syncErr := destinationFile.Sync()
			closeErr := destinationFile.Close()
			if errCopy == nil { // Only assign sync/close error if copy was fine
				if syncErr != nil {
					errCopy = syncErr
				} else if closeErr != nil {
					errCopy = closeErr
				}
			}
		}
		b.StartTimer() // Resume timer briefly so the loop iteration cost is captured

		if errCopy != nil {
			b.Fatalf("Copy or close/sync failed: %v", errCopy)
		}
	}
	b.StopTimer()
	err := os.Remove(dstPath)
	if err != nil && !os.IsNotExist(err) {
		b.Logf("Warning: failed to remove destination file %s after benchmark: %v", dstPath, err)
	}
}

func BenchmarkCopyFile_Buffer4KB(b *testing.B) {
	runBenchmark(b, "4KB", 4*1024)
}

func BenchmarkCopyFile_Buffer32KB(b *testing.B) {
	runBenchmark(b, "32KB", 32*1024)
}

func BenchmarkCopyFile_Buffer64KB(b *testing.B) {
	runBenchmark(b, "64KB", 64*1024)
}

func BenchmarkCopyFile_Buffer128KB(b *testing.B) {
	runBenchmark(b, "128KB", 128*1024)
}

func BenchmarkCopyFile_Buffer256KB(b *testing.B) {
	runBenchmark(b, "256KB", 256*1024)
}

func BenchmarkCopyFile_Buffer512KB(b *testing.B) {
	runBenchmark(b, "512KB", 512*1024)
}

func BenchmarkCopyFile_Buffer1MB(b *testing.B) {
	runBenchmark(b, "1MB", 1024*1024)
}

func BenchmarkCopyFile_Buffer4MB(b *testing.B) {
	runBenchmark(b, "4MB", 4*1024*1024)
}

func BenchmarkCopyFile_Buffer8MB(b *testing.B) {
	runBenchmark(b, "8MB", 8*1024*1024)
}

// TestMain is used to set up and tear down test environment.
// Here, we use it to load source file information once.
func TestMain(m *testing.M) {
	// Perform setup here
	fmt.Println("Loading source file information...")
	loadSourceInfo()

	if sourceInfoErr != nil {
		fmt.Fprintf(os.Stderr, "ERROR during benchmark setup: %v\n", sourceInfoErr)
		fmt.Fprintf(os.Stderr, "Please ensure 'sourceFilePath' (%s) and 'targetDir' (%s) are correctly configured and accessible.\n", sourceFilePath, targetDir)
		// We could os.Exit(1) here, but letting benchmarks run and fail
		// individually also shows the problem. For CI, exiting might be better.
	} else if globalSourceFileSize > 0 {
		fmt.Printf("Source file: %s, Size: %.2f MB\n", sourceFilePath, float64(globalSourceFileSize)/(1024*1024))
		fmt.Printf("Target directory for copies: %s\n", targetDir)
	} else if sourceInfoErr == nil { // No error but size is 0
		fmt.Fprintf(os.Stderr, "Warning: Source file '%s' is empty or size could not be determined correctly.\n", sourceFilePath)
	}

	// Run the benchmarks
	exitCode := m.Run()

	// Perform teardown here if needed (e.g., cleaning up commonly created files, though
	// individual benchmarks should clean up their own specific files).
	// In this case, individual benchmarks clean their own .tmp files.

	os.Exit(exitCode)
}
