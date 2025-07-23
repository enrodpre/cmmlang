#pragma once

#include <memory>
#include <string>
#include <vector>

// Forward decls
class Operand;
class Register;
class Immediate;
class Memory;

// Base operand class
class Operand {
public:
  enum class Type { REGISTER, IMMEDIATE, MEMORY, LABEL };

  enum class Size {
    BYTE  = 8,   // 8-bit
    WORD  = 16,  // 16-bit
    DWORD = 32,  // 32-bit
    QWORD = 64,  // 64-bit
    TBYTE = 80,  // 80-bit (x87)
    OWORD = 128, // 128-bit (XMM)
    YWORD = 256, // 256-bit (YMM)
    ZWORD = 512  // 512-bit (ZMM)
  };

  virtual ~Operand()                             = default;
  virtual Type getType() const                   = 0;
  virtual Size getSize() const                   = 0;
  virtual std::string toString() const           = 0;
  virtual std::unique_ptr<Operand> clone() const = 0;
};

// =============================================================================
// REGISTER OPERANDS
// =============================================================================

class Register : public Operand {
public:
  enum class Category {
    GENERAL_PURPOSE,
    SEGMENT,
    CONTROL,
    DEBUG,
    TEST,
    FPU,
    MMX,
    XMM,
    YMM,
    ZMM,
    BOUND,
    OPMASK
  };

  Register(const std::string& name,
           Size size,
           Category category,
           int index = -1)
      : name_(name),
        size_(size),
        category_(category),
        index_(index) {}

  Type getType() const override { return Type::REGISTER; }
  Size getSize() const override { return size_; }
  Category getCategory() const { return category_; }
  int getIndex() const { return index_; }
  std::string getName() const { return name_; }
  std::string toString() const override { return name_; }

protected:
  std::string name_;
  Size size_;
  Category category_;
  int index_;
};

// General Purpose Registers
class GeneralPurposeRegister : public Register {
public:
  GeneralPurposeRegister(const std::string& name, Size size, int index)
      : Register(name, size, Category::GENERAL_PURPOSE, index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<GeneralPurposeRegister>(*this);
  }

  // Helper methods to get different sized versions
  std::unique_ptr<GeneralPurposeRegister> get8BitLow() const;
  std::unique_ptr<GeneralPurposeRegister> get8BitHigh() const;
  std::unique_ptr<GeneralPurposeRegister> get16Bit() const;
  std::unique_ptr<GeneralPurposeRegister> get32Bit() const;
  std::unique_ptr<GeneralPurposeRegister> get64Bit() const;
};

// Segment Registers
class SegmentRegister : public Register {
public:
  enum class Segment { CS, DS, ES, FS, GS, SS };

  SegmentRegister(Segment seg)
      : Register(segmentToString(seg), Size::WORD, Category::SEGMENT),
        segment_(seg) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<SegmentRegister>(*this);
  }

  Segment getSegment() const { return segment_; }

private:
  Segment segment_;
  static std::string segmentToString(Segment seg);
};

// Control Registers
class ControlRegister : public Register {
public:
  ControlRegister(int index)
      : Register("CR" + std::to_string(index),
                 Size::QWORD,
                 Category::CONTROL,
                 index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<ControlRegister>(*this);
  }
};

// Debug Registers
class DebugRegister : public Register {
public:
  DebugRegister(int index)
      : Register("DR" + std::to_string(index),
                 Size::QWORD,
                 Category::DEBUG,
                 index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<DebugRegister>(*this);
  }
};

// FPU Registers
class FPURegister : public Register {
public:
  FPURegister(int index)
      : Register("ST" + std::to_string(index),
                 Size::TBYTE,
                 Category::FPU,
                 index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<FPURegister>(*this);
  }
};

// MMX Registers
class MMXRegister : public Register {
public:
  MMXRegister(int index)
      : Register("MM" + std::to_string(index),
                 Size::QWORD,
                 Category::MMX,
                 index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<MMXRegister>(*this);
  }
};

// XMM Registers (128-bit)
class XMMRegister : public Register {
public:
  XMMRegister(int index)
      : Register("XMM" + std::to_string(index),
                 Size::OWORD,
                 Category::XMM,
                 index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<XMMRegister>(*this);
  }
};

// YMM Registers (256-bit)
class YMMRegister : public Register {
public:
  YMMRegister(int index)
      : Register("YMM" + std::to_string(index),
                 Size::YWORD,
                 Category::YMM,
                 index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<YMMRegister>(*this);
  }
};

// ZMM Registers (512-bit)
class ZMMRegister : public Register {
public:
  ZMMRegister(int index)
      : Register("ZMM" + std::to_string(index),
                 Size::ZWORD,
                 Category::ZMM,
                 index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<ZMMRegister>(*this);
  }
};

// Opmask Registers (AVX-512)
class OpmaskRegister : public Register {
public:
  OpmaskRegister(int index)
      : Register("K" + std::to_string(index),
                 Size::QWORD,
                 Category::OPMASK,
                 index) {}

  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<OpmaskRegister>(*this);
  }
};

// =============================================================================
// IMMEDIATE OPERANDS
// =============================================================================

class Immediate : public Operand {
public:
  enum class ImmediateType {
    SIGNED_INTEGER,
    UNSIGNED_INTEGER,
    FLOAT,
    DOUBLE,
    LONG_DOUBLE
  };

  Immediate(int64_t value, Size size = Size::DWORD)
      : value_(value),
        size_(size),
        type_(ImmediateType::SIGNED_INTEGER) {}

  Immediate(uint64_t value, Size size = Size::DWORD)
      : uvalue_(value),
        size_(size),
        type_(ImmediateType::UNSIGNED_INTEGER) {}

  Type getType() const override { return Type::IMMEDIATE; }
  Size getSize() const override { return size_; }
  ImmediateType getImmediateType() const { return type_; }

  std::string toString() const override;
  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<Immediate>(*this);
  }

  int64_t getSignedValue() const { return value_; }
  uint64_t getUnsignedValue() const { return uvalue_; }

private:
  union {
    int64_t value_;
    uint64_t uvalue_;
    double fvalue_;
    long double ldvalue_;
  };
  Size size_;
  ImmediateType type_;
};

// =============================================================================
// MEMORY OPERANDS
// =============================================================================

class Memory : public Operand {
public:
  Memory(Size size = Size::DWORD)
      : size_(size),
        base_(nullptr),
        index_(nullptr),
        scale_(1),
        displacement_(0),
        segment_(nullptr) {}

  Type getType() const override { return Type::MEMORY; }
  Size getSize() const override { return size_; }

  // Builder pattern methods
  Memory& setBase(std::unique_ptr<Register> base) {
    base_ = std::move(base);
    return *this;
  }

  Memory& setIndex(std::unique_ptr<Register> index) {
    index_ = std::move(index);
    return *this;
  }

  Memory& setScale(int scale) {
    if (scale == 1 || scale == 2 || scale == 4 || scale == 8) {
      scale_ = scale;
    }
    return *this;
  }

  Memory& setDisplacement(int64_t displacement) {
    displacement_ = displacement;
    return *this;
  }

  Memory& setSegment(std::unique_ptr<SegmentRegister> segment) {
    segment_ = std::move(segment);
    return *this;
  }

  Memory& setSize(Size size) {
    size_ = size;
    return *this;
  }

  std::string toString() const override;
  std::unique_ptr<Operand> clone() const override;

  // Getters
  const Register* getBase() const { return base_.get(); }
  const Register* getIndex() const { return index_.get(); }
  int getScale() const { return scale_; }
  int64_t getDisplacement() const { return displacement_; }
  const SegmentRegister* getSegment() const { return segment_.get(); }

private:
  Size size_;
  std::unique_ptr<Register> base_;
  std::unique_ptr<Register> index_;
  int scale_;
  int64_t displacement_;
  std::unique_ptr<SegmentRegister> segment_;
};

// Specialized memory operand types
class DirectMemory : public Memory {
public:
  DirectMemory(uint64_t address, Size size = Size::DWORD)
      : Memory(size),
        address_(address) {}

  std::string toString() const override;
  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<DirectMemory>(*this);
  }

  uint64_t getAddress() const { return address_; }

private:
  uint64_t address_;
};

// =============================================================================
// LABEL OPERANDS
// =============================================================================

class Label : public Operand {
public:
  Label(const std::string& name)
      : name_(name) {}

  Type getType() const override { return Type::LABEL; }
  Size getSize() const override { return Size::QWORD; } // Address size
  std::string toString() const override { return name_; }
  std::unique_ptr<Operand> clone() const override {
    return std::make_unique<Label>(*this);
  }

  std::string getName() const { return name_; }

private:
  std::string name_;
};

// =============================================================================
// FACTORY FUNCTIONS AND UTILITIES
// =============================================================================

class OperandFactory {
public:
  // General purpose registers
  static std::unique_ptr<GeneralPurposeRegister> RAX() {
    return std::make_unique<GeneralPurposeRegister>(
        "RAX", Operand::Size::QWORD, 0);
  }
  static std::unique_ptr<GeneralPurposeRegister> EAX() {
    return std::make_unique<GeneralPurposeRegister>(
        "EAX", Operand::Size::DWORD, 0);
  }
  static std::unique_ptr<GeneralPurposeRegister> AX() {
    return std::make_unique<GeneralPurposeRegister>(
        "AX", Operand::Size::WORD, 0);
  }
  static std::unique_ptr<GeneralPurposeRegister> AL() {
    return std::make_unique<GeneralPurposeRegister>(
        "AL", Operand::Size::BYTE, 0);
  }
  static std::unique_ptr<GeneralPurposeRegister> AH() {
    return std::make_unique<GeneralPurposeRegister>(
        "AH", Operand::Size::BYTE, 0);
  }

  // Similar factories for RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8-R15...

  // Segment registers
  static std::unique_ptr<SegmentRegister> CS() {
    return std::make_unique<SegmentRegister>(SegmentRegister::Segment::CS);
  }
  static std::unique_ptr<SegmentRegister> DS() {
    return std::make_unique<SegmentRegister>(SegmentRegister::Segment::DS);
  }
  // ... other segment registers

  // XMM registers
  static std::unique_ptr<XMMRegister> XMM(int index) {
    return std::make_unique<XMMRegister>(index);
  }

  // Memory operands
  static std::unique_ptr<Memory> Mem(
      Operand::Size size = Operand::Size::DWORD) {
    return std::make_unique<Memory>(size);
  }

  // Immediate values
  static std::unique_ptr<Immediate> Imm(
      int64_t value,
      Operand::Size size = Operand::Size::DWORD) {
    return std::make_unique<Immediate>(value, size);
  }

  // Labels
  static std::unique_ptr<Label> Lbl(const std::string& name) {
    return std::make_unique<Label>(name);
  }
};

// Helper function to get size suffix for NASM
std::string getSizeSuffix(Operand::Size size) {
  switch (size) {
    case Operand::Size::BYTE:
      return "BYTE";
    case Operand::Size::WORD:
      return "WORD";
    case Operand::Size::DWORD:
      return "DWORD";
    case Operand::Size::QWORD:
      return "QWORD";
    case Operand::Size::TBYTE:
      return "TBYTE";
    case Operand::Size::OWORD:
      return "OWORD";
    case Operand::Size::YWORD:
      return "YWORD";
    case Operand::Size::ZWORD:
      return "ZWORD";
    default:
      return "";
  }
}
