#include "Diagnostic.h"
#include <llvm/ADT/SmallString.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

Diagnostic::Diagnostic() : OS(errs()) {}

void Diagnostic::backtrace(const Instruction *I) {
    const DILocation *Loc = I->getDebugLoc();
    if (!Loc)
        return;
    OS << "stack: \n";
    while (Loc) {
        this->location(Loc);
        Loc = Loc->getInlinedAt();
    }
}

void Diagnostic::location(const DILocation *Loc) {
    if (!Loc)
        return;
    SmallString<64> Path;
    StringRef Filename = Loc->getFilename();
    if (sys::path::is_absolute(Filename))
        Path.append(Filename.begin(), Filename.end());
    else
        sys::path::append(Path, Loc->getDirectory(), Filename);
    OS << "  - " << Path << ':' << Loc->getLine() << ':' << Loc->getColumn()
       << "\n";
}

void Diagnostic::bug(Instruction *I) {
    MDNode *MD = I->getMetadata("bug");
    if (!MD)
        return;
    this->bug(cast<MDString>(MD->getOperand(0))->getString());
}

void Diagnostic::bug(const Twine &Str) {
    OS << "---\n"
       << "bug: " << Str << "\n";
}

void Diagnostic::status(const Optional<bool> &Status) {
    const char *Str;
    if (Status.hasValue()) {
        if (Status.getValue()) {
            Str = "sat";
        } else {
            Str = "unsat";
        }
    } else {
        Str = "undef";
    }
    OS << "status: " << Str << "\n";
}
