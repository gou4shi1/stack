#pragma once

#include <llvm/ADT/Optional.h>

namespace llvm {

class Instruction;
class DILocation;
class raw_ostream;
class Twine;

} // namespace llvm

class Diagnostic {
  public:
    Diagnostic();

    llvm::raw_ostream &os() { return OS; }

    void bug(llvm::Instruction *);
    void bug(const llvm::Twine &);

    void backtrace(llvm::Instruction *);
    void location(const llvm::DILocation *);
    void status(const llvm::Optional<bool> &Status);

    template <typename T> Diagnostic &operator<<(const T &Val) {
        OS << Val;
        return *this;
    }

  private:
    llvm::raw_ostream &OS;
};
