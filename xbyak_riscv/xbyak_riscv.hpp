#pragma once
/*!
	@file xbyak_riscv.hpp
	@brief Xbyak_riscv ; JIT assembler for RISC-V
	@author herumi
	@url https://github.com/herumi/xbyak_riscv
	@note modified new BSD license
	http://opensource.org/licenses/BSD-3-Clause
*/

// Copyright (C), 2023, KNS Group LLC (YADRO)

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <list>
#include <string>
#include <algorithm>
#include <unordered_set>
#include <unordered_map>

#define XBYAK_RISCV_ASSERT(x) assert(x)

#include "xbyak_riscv_csr.hpp"

namespace Xbyak_riscv {

enum {
	DEFAULT_MAX_CODE_SIZE = 4096,
	VERSION = 0x0010 /* 0xABCD = A.BC.D */
};

inline uint32_t getVersion() { return VERSION; }

enum {
	ERR_NONE = 1,
	ERR_OFFSET_IS_TOO_BIG,
	ERR_CODE_IS_TOO_BIG,
	ERR_IMM_IS_TOO_BIG,
	ERR_INVALID_IMM_OF_JAL,
	ERR_INVALID_IMM_OF_BTYPE,
	ERR_LABEL_IS_NOT_FOUND,
	ERR_LABEL_IS_REDEFINED,
	ERR_LABEL_IS_TOO_FAR,
	ERR_LABEL_IS_NOT_SET_BY_L,
	ERR_LABEL_IS_ALREADY_SET_BY_L,
	ERR_CANT_PROTECT,
	ERR_CANT_ALLOC,
	ERR_BAD_PARAMETER,
	ERR_MUNMAP,
	ERR_INTERNAL // Put it at last.
};

inline const char *ConvertErrorToString(int err)
{
	static const char *errTbl[] = {
		"none",
		"offset is too big",
		"code is too big",
		"imm is too big",
		"invalid imm of jal",
		"invalid imm of Btype",
		"label is not found",
		"label is redefined",
		"label is too far",
		"label is not set by L",
		"label is already set by L",
		"can't protect",
		"can't alloc",
		"bad parameter",
		"munmap",
		"internal error"
	};
	assert(ERR_INTERNAL == sizeof(errTbl) / sizeof(*errTbl));
	return err <= ERR_INTERNAL ? errTbl[err] : "unknown err";
}

class Error : public std::exception {
	int err_;
public:
	explicit Error(int err) : err_(err)
	{
		if (err_ < 0 || err_ > ERR_INTERNAL) {
			err_ = ERR_INTERNAL;
		}
	}
	operator int() const { return err_; }
	const char *what() const noexcept
	{
		return ConvertErrorToString(err_);
	}
};

// dummy functions
inline void ClearError() { }
inline int GetError() { return 0; }

inline const char *ConvertErrorToString(const Error& err)
{
	return err.what();
}

#define XBYAK_RISCV_THROW(err) { throw Error(err); }
#define XBYAK_RISCV_THROW_RET(err, r) { throw Error(err); }


namespace local {

inline constexpr uint32_t mask(size_t n)
{
	XBYAK_RISCV_ASSERT(n <= 32);
	return n == 32 ? 0xffffffff : (1u << n) - 1;
}
// is x <= mask(n) ?
inline constexpr bool inBit(uint32_t x, size_t n)
{
	return x <= mask(n);
}

// is x a signed n-bit integer?
inline constexpr bool inSBit(int x, int n)
{
	return -(1 << (n-1)) <= x && x < (1 << (n-1));
}

// split x to hi20bits and low12bits
// return false if x in 12-bit signed integer
inline bool split32bit(int *pH, int* pL, int x) {
	if (inSBit(x, 12)) return false;
	int H = (x >> 12) & mask(20);
	int L = x & mask(12);
	if (x & (1 << 11)) {
		H++;
		L = L | (mask(20) << 12);
	}
	*pH = H;
	*pL = L;
	return true;
}

// @@@ embedded by bit_pattern.py (DON'T DELETE THIS LINE)
inline size_t get20_10to1_11_19to12_z12(size_t v) { return ((v & (1<<20)) << 11)| ((v & (1023<<1)) << 20)| ((v & (1<<11)) << 9)| (v & (255<<12)); }
inline size_t get12_10to5_z13_4to1_11_z7(size_t v) { return ((v & (1<<12)) << 19)| ((v & (63<<5)) << 20)| ((v & (15<<1)) << 7)| ((v & (1<<11)) >> 4); }
inline size_t get5to4_9to6_2_3_z5(size_t v) { return ((v & (3<<4)) << 7)| ((v & (15<<6)) << 1)| ((v & (1<<2)) << 4)| ((v & (1<<3)) << 2); }
inline size_t get9_z5_4_6_8to7_5_z2(size_t v) { return ((v & (1<<9)) << 3)| ((v & (1<<4)) << 2)| ((v & (1<<6)) >> 1)| ((v & (3<<7)) >> 4)| ((v & (1<<5)) >> 3); }
inline size_t get5to3_z3_2_6_z5(size_t v) { return ((v & (7<<3)) << 7)| ((v & (1<<2)) << 4)| ((v & (1<<6)) >> 1); }
inline size_t get5to3_z3_7_6_z5(size_t v) { return ((v & (7<<3)) << 7)| ((v & (1<<7)) >> 1)| ((v & (1<<6)) >> 1); }
inline size_t get5_z5_4to0_z2(size_t v) { return ((v & (1<<5)) << 7)| ((v & 31) << 2); }
inline size_t get11_4_9to8_10_6_7_3to1_5_z2(size_t v) { return ((v & (1<<11)) << 1)| ((v & (1<<4)) << 7)| ((v & (3<<8)) << 1)| ((v & (1<<10)) >> 2)| ((v & (1<<6)) << 1)| ((v & (1<<7)) >> 1)| ((v & (7<<1)) << 2)| ((v & (1<<5)) >> 3); }
inline size_t get17_z5_16to12_z2(size_t v) { return ((v & (1<<17)) >> 5)| ((v & (31<<12)) >> 10); }
inline size_t get5_z5_4to2_7to6_z2(size_t v) { return ((v & (1<<5)) << 7)| ((v & (7<<2)) << 2)| ((v & (3<<6)) >> 4); }
inline size_t get5_z5_4to3_8to6_z2(size_t v) { return ((v & (1<<5)) << 7)| ((v & (3<<3)) << 2)| ((v & (7<<6)) >> 4); }
inline size_t get5to2_7to6_z7(size_t v) { return ((v & (15<<2)) << 7)| ((v & (3<<6)) << 1); }
inline size_t get5to3_8to6_z7(size_t v) { return ((v & (7<<3)) << 7)| ((v & (7<<6)) << 1); }
// @@@ embedded by bit_pattern.py (DON'T DELETE THIS LINE)

} // local


namespace local {

// Register Interface
class IReg {
public:
	enum Kind {
		GPR = 1,         // General purpose register
		FReg = 1 << 1,   // Floating-point register
		VECTOR = 1 << 2, // Vector register
	};
protected:
	uint32_t idx_;
	Kind kind_;
public:
	constexpr IReg(uint32_t idx = 0, Kind kind = GPR)
		: idx_(idx), kind_(kind)
	{
		XBYAK_RISCV_ASSERT(local::inBit(idx, 5));
	}
	constexpr int getIdx() const { return idx_; }
	const char *toString() const
	{
		if (kind_ == GPR) {
			static const char tbl[][4] = {
				"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
				"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
				"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
				"x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31",
			};
			return tbl[idx_];
		} else if (kind_ == FReg) {
			static const char tbl[][4] = {
				"f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
				"f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",
				"f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",
				"f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",
			};
			return tbl[idx_];
		} else if (kind_ == VECTOR) {
			static const char tbl[][4] = {
				"v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7",
				"v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15",
				"v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
				"v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31",
			};
			return tbl[idx_];
		}
		XBYAK_RISCV_THROW_RET(ERR_INTERNAL, 0);
	}
	bool operator==(const IReg& rhs) const
	{
		return idx_ == rhs.idx_ && kind_ == rhs.kind_;
	}
	bool operator!=(const IReg& rhs) const { return !operator==(rhs); }

};

} // local

// General Purpose Register
struct Reg : public local::IReg {
	explicit constexpr Reg(int idx = 0) : local::IReg(idx, IReg::Kind::GPR) { }
};

static constexpr Reg x0(0), x1(1), x2(2), x3(3), x4(4), x5(5), x6(6), x7(7);
static constexpr Reg x8(8), x9(9), x10(10), x11(11), x12(12), x13(13), x14(14), x15(15);
static constexpr Reg x16(16), x17(17), x18(18), x19(19), x20(20), x21(21), x22(22), x23(23);
static constexpr Reg x24(24), x25(25), x26(26), x27(27), x28(28), x29(29), x30(30), x31(31);

static constexpr Reg zero(x0);
static constexpr Reg ra(x1);
static constexpr Reg sp(x2);
static constexpr Reg gp(x3);
static constexpr Reg tp(x4);
static constexpr Reg t0(x5);
static constexpr Reg t1(x6);
static constexpr Reg t2(x7);
static constexpr Reg fp(x8);
static constexpr Reg s0(x8);
static constexpr Reg s1(x9);
static constexpr Reg a0(x10), a1(x11), a2(x12), a3(x13), a4(x14), a5(x15), a6(x16), a7(x17);
static constexpr Reg s2(x18), s3(x19), s4(x20), s5(x21), s6(x22), s7(x23), s8(x24), s9(x25);
static constexpr Reg s10(x26), s11(x27);
static constexpr Reg t3(x28), t4(x29), t5(x30), t6(x31);

class CodeArray {
	std::vector<uint32_t> code_;
public:

	void resetSize()
	{
		code_.clear();
	}

	void append4B(uint32_t code) { code_.push_back(code); }
	
	void write4B(size_t offset, uint32_t v) { code_.at(offset) = v; }
	void dump(bool separate = false) const
	{
		const uint8_t *p = getCode();
		const size_t bufSize = getSize();
		if (separate) {
			size_t pos = 0;
			while (pos < bufSize) {
				uint32_t v = p[pos];
				size_t n = (v & 3) == 3 ? 4 : 2;
				if (pos + n <= bufSize) {
					for (size_t i = 0; i < n; i++) {
						printf("%02x", p[pos + n - 1 - i]);
					}
					printf("\n");
					pos += n;
				} else {
					printf("%02x error\n", v);
					return;
				}
			}
			return;
		}
		size_t remain = bufSize;
		for (int i = 0; i < 4; i++) {
			size_t disp = 16;
			if (remain < 16) {
				disp = remain;
			}
			for (size_t j = 0; j < 16; j++) {
				if (j < disp) {
					printf("%02x", p[i * 16 + j]);
				}
			}
			putchar('\n');
			remain -= disp;
			if (remain == 0) {
				break;
			}
		}
	}
};

struct Jmp {
	enum Type {
		tJal,
		tBtype,
		tRawAddress,
	} type;
	const uint8_t* from; /* address of the jmp mnemonic */
	uint32_t encoded;
	size_t encSize() const
	{
		return (type == tRawAddress) ? sizeof(size_t) : 4;
	}
	// jal
	Jmp(const uint8_t *from, uint32_t opcode, const Reg& rd)
		: type(tJal)
		, from(from)
		, encoded((rd.getIdx() << 7) | opcode)
	{
	}
	// B-type
	Jmp(const uint8_t* from, uint32_t opcode, uint32_t funct3, const Reg& src1, const Reg& src2)
		: type(tBtype)
		, from(from)
		, encoded((src2.getIdx() << 20) | (src1.getIdx() << 15) | (funct3 << 12) | opcode)
	{
	}
	// raw address
	explicit Jmp(const uint8_t* from)
		: type(tRawAddress)
		, from(from)
		, encoded(0)
	{
	}
	static inline bool isValidImm(size_t imm, size_t maskBit)
	{
		const size_t M = local::mask(maskBit);
		return (imm < M || ~M <= imm) && (imm & 1) == 0;
	}
	size_t encode(const uint8_t* addr) const
	{
		if (addr == 0) return 0;
		if (type == tRawAddress) return size_t(addr);
		const size_t imm = addr - from;
		if (type == tJal) {
			if (!isValidImm(imm, 20)) XBYAK_RISCV_THROW(ERR_INVALID_IMM_OF_JAL)
			return local::get20_10to1_11_19to12_z12(imm) | encoded;
		} else {
			if (!isValidImm(imm, 12)) XBYAK_RISCV_THROW(ERR_INVALID_IMM_OF_JAL)
			return local::get12_10to5_z13_4to1_11_z7(imm) | encoded;
		}
	}
	// update jmp address by base->getCurr()
	void update(CodeArray *base) const
	{
		base->writeBytes(from, encode(base->getCurr()), encSize());
	}
	// append jmp opcode with addr
	void appendCode(CodeArray *base, const uint8_t *addr) const
	{
		base->appendBytes(encode(addr), encSize());
	}
};

class LabelManager;

class Label {
	mutable LabelManager *mgr;
	mutable int id;
	friend class LabelManager;
public:
	Label() : mgr(0), id(0) {}
	Label(const Label& rhs);
	Label& operator=(const Label& rhs);
	~Label();
	void clear() { mgr = 0; id = 0; }
	int getId() const { return id; }
	const uint8_t *getAddress() const;
};

class LabelManager {
	// for Label class
	struct ClabelVal {
		ClabelVal(const uint8_t* addr = 0) : addr(addr), refCount(1) {}
		const uint8_t* addr;
		int refCount;
	};
	typedef std::unordered_map<int, ClabelVal> ClabelDefList;
	typedef std::unordered_multimap<int, Jmp> ClabelUndefList;
	typedef std::unordered_set<Label*> LabelPtrList;

	CodeArray *base_;
	mutable int labelId_;
	ClabelDefList clabelDefList_;
	ClabelUndefList clabelUndefList_;
	LabelPtrList labelPtrList_;

	int getId(const Label& label) const
	{
		if (label.id == 0) label.id = labelId_++;
		return label.id;
	}
	void define_inner(ClabelDefList& defList, ClabelUndefList& undefList, int labelId, const uint8_t* addr)
	{
		// add label
		ClabelDefList::value_type item(labelId, addr);
		std::pair<ClabelDefList::iterator, bool> ret = defList.insert(item);
		if (!ret.second) XBYAK_RISCV_THROW(ERR_LABEL_IS_REDEFINED)
		// search undefined label
		for (;;) {
			ClabelUndefList::iterator itr = undefList.find(labelId);
			if (itr == undefList.end()) break;
			const Jmp& jmp = itr->second;
			jmp.update(base_);
			undefList.erase(itr);
		}
	}
	friend class Label;
	void incRefCount(int id, Label *label)
	{
		clabelDefList_[id].refCount++;
		labelPtrList_.insert(label);
	}
	void decRefCount(int id, Label *label)
	{
		labelPtrList_.erase(label);
		ClabelDefList::iterator i = clabelDefList_.find(id);
		if (i == clabelDefList_.end()) return;
		if (i->second.refCount == 1) {
			clabelDefList_.erase(id);
		} else {
			--i->second.refCount;
		}
	}
	template<class T>
	bool hasUndefinedLabel_inner(const T& list) const
	{
		return !list.empty();
	}
	// detach all labels linked to LabelManager
	void resetLabelPtrList()
	{
		for (LabelPtrList::iterator i = labelPtrList_.begin(), ie = labelPtrList_.end(); i != ie; ++i) {
			(*i)->clear();
		}
		labelPtrList_.clear();
	}
public:
	LabelManager()
	{
		reset();
	}
	~LabelManager()
	{
		resetLabelPtrList();
	}
	void reset()
	{
		base_ = 0;
		labelId_ = 1;
		clabelDefList_.clear();
		clabelUndefList_.clear();
		resetLabelPtrList();
	}
	void set(CodeArray *base) { base_ = base; }
	void defineClabel(Label& label)
	{
		define_inner(clabelDefList_, clabelUndefList_, getId(label), base_->getCurr());
		label.mgr = this;
		labelPtrList_.insert(&label);
	}
	void assign(Label& dst, const Label& src)
	{
		ClabelDefList::const_iterator i = clabelDefList_.find(src.id);
		if (i == clabelDefList_.end()) XBYAK_RISCV_THROW(ERR_LABEL_IS_NOT_SET_BY_L)
		define_inner(clabelDefList_, clabelUndefList_, dst.id, i->second.addr);
		dst.mgr = this;
		labelPtrList_.insert(&dst);
	}
	// return 0 unless label exists
	const uint8_t* getAddr(const Label& label) const
	{
		ClabelDefList::const_iterator i = clabelDefList_.find(getId(label));
		if (i == clabelDefList_.end()) return 0;
		return i->second.addr;
	}
	void addUndefinedLabel(const Label& label, const Jmp& jmp)
	{
		clabelUndefList_.insert(ClabelUndefList::value_type(label.id, jmp));
	}
	bool hasUndefClabel() const { return hasUndefinedLabel_inner(clabelUndefList_); }
	const uint8_t *getCode() const { return base_->getCode(); }
};

inline Label::Label(const Label& rhs)
{
	id = rhs.id;
	mgr = rhs.mgr;
	if (mgr) mgr->incRefCount(id, this);
}
inline Label& Label::operator=(const Label& rhs)
{
	if (id) XBYAK_RISCV_THROW_RET(ERR_LABEL_IS_ALREADY_SET_BY_L, *this)
	id = rhs.id;
	mgr = rhs.mgr;
	if (mgr) mgr->incRefCount(id, this);
	return *this;
}
inline Label::~Label()
{
	if (id && mgr) mgr->decRefCount(id, this);
}
inline const uint8_t* Label::getAddress() const
{
	if (mgr == 0) return 0;
	return mgr->getAddr(*this);
}

namespace local {

template<size_t n>
struct Bit {
	uint32_t v;
	Bit(uint32_t v)
		: v(v)
	{
		XBYAK_RISCV_ASSERT(inBit(v, n));
	}
	Bit(const IReg& r)
		: v(r.getIdx())
	{
	}
	Bit(VM vm)
		: v(static_cast<uint32_t>(vm))
	{
	}
	Bit(CSR csr)
		: v(static_cast<uint32_t>(csr))
	{
	}
	Bit(RM rm)
		: v(static_cast<uint32_t>(rm))
	{
	}
};

} // local

class CodeGenerator : public CodeArray {
public:
	enum AqRlType {
		T_aq = 2,
		T_rl = 1,
		T_aqrl = 3,
	};
	typedef local::Bit<1> Bit1;
	typedef local::Bit<2> Bit2;
	typedef local::Bit<3> Bit3;
	typedef local::Bit<5> Bit5;
	typedef local::Bit<6> Bit6;
	typedef local::Bit<7> Bit7;
	typedef local::Bit<12> Bit12;
	typedef local::Bit<32> Bit32;
private:
	CodeGenerator operator=(const CodeGenerator&) = delete;
	LabelManager labelMgr_;
    
	void opJmp(const Label& label, const Jmp& jmp)
	{
		const uint8_t* addr = labelMgr_.getAddr(label);
		jmp.appendCode(this, addr);
		if (addr) return;
		labelMgr_.addUndefinedLabel(label, jmp);
	}
	uint32_t enc2(uint32_t a, uint32_t b) const { return (a<<7) | (b<<15); }
	uint32_t enc3(uint32_t a, uint32_t b, uint32_t c) const { return enc2(a, b) | (c<<20); }
	void Rtype(Bit7 opcode, Bit3 funct3, Bit7 funct7, Bit5 rd, Bit5 rs1, Bit5 rs2)
	{
		uint32_t v = (funct7.v<<25) | (funct3.v<<12) | opcode.v | enc3(rd.v, rs1.v, rs2.v);
		append4B(v);
	}
	void Itype(Bit7 opcode, Bit3 funct3, Bit5 rd, Bit5 rs1, int imm)
	{
		if (!local::inSBit(imm, 12)) XBYAK_RISCV_THROW(ERR_IMM_IS_TOO_BIG)
		uint32_t v = (imm<<20) | (funct3.v<<12) | opcode.v | enc2(rd.v, rs1.v);
		append4B(v);
	}
	void Stype(Bit7 opcode, Bit3 funct3, Bit5 rs1, Bit5 rs2, int imm)
	{
		if (!local::inSBit(imm, 12)) XBYAK_RISCV_THROW(ERR_IMM_IS_TOO_BIG)
		uint32_t v = ((imm>>5)<<25) | (funct3.v<<12) | opcode.v | enc3(imm & local::mask(5), rs1.v, rs2.v);
		append4B(v);
	}
	void Utype(Bit7 opcode, Bit5 rd, uint32_t imm)
	{
		if (imm >= (1u << 20)) XBYAK_RISCV_THROW(ERR_IMM_IS_TOO_BIG)
		uint32_t v = (imm<<12) | opcode.v | (rd.v<<7);
		append4B(v);
	}
	void opShift(Bit7 pre, Bit3 funct3, Bit7 opcode, Bit5 rd, Bit5 rs1, uint32_t shamt, int range = 0)
	{
		if (range == 0) range = 5;
		if (shamt >= (1u << range)) XBYAK_RISCV_THROW(ERR_IMM_IS_TOO_BIG)
		uint32_t v = (pre.v<<25) | (funct3.v<<12) | opcode.v | enc3(rd.v, rs1.v, shamt);
		append4B(v);
	}
	void opAtomic(Bit5 rd, Bit5 rs2, Bit5 addr, Bit5 funct5, Bit3 funct3, uint32_t flag)
	{
		assert(flag <= 3);
		Rtype(0x2f, funct3.v, (funct5.v << 2) | flag, rd, addr, rs2);
	}
	void opIVV(Bit32 baseValue, Bit1 vm, Bit5 vs2, Bit5 vs1, Bit5 vd)
	{
		/*
		    31 .. 26 | 25 | 24 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			  func6    vm      vs2       vs1        func3       vd     opcode

			func6, func3, and opcode must be encoded in the baseValue
		*/
		uint32_t v = (vm.v<<25) | (vs2.v<<20) | (vs1.v<<15) | (vd.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	void opFVV(Bit32 baseValue, Bit1 vm, Bit5 vs2, Bit5 vs1, Bit5 d)
	{
		/*
		    31 .. 26 | 25 | 24 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			  func6    vm      vs2       vs1        func3     vd/rd    opcode

			func6, func3, and opcode must be encoded in the baseValue
		*/
		uint32_t v = (vm.v<<25) | (vs2.v<<20) | (vs1.v<<15) | (d.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	void opMVV(Bit32 baseValue, Bit1 vm, Bit5 vs2, Bit5 vs1, Bit5 d)
	{
		/*
		    31 .. 26 | 25 | 24 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			  func6    vm      vs2       vs1        func3     vd/rd    opcode

			func6, func3, and opcode must be encoded in the baseValue
		*/
		uint32_t v = (vm.v<<25) | (vs2.v<<20) | (vs1.v<<15) | (d.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	void opIVI(Bit32 baseValue, Bit1 vm, Bit5 vs2, uint32_t imm, Bit5 vd)
	{
		/*
		    31 .. 26 | 25 | 24 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			  func6    vm      vs2       imm       func3       vd     opcode

			func6, func3, and opcode must be encoded in the baseValue
		*/
		uint32_t v = (vm.v<<25) | (vs2.v<<20) | ((imm & local::mask(5))<<15) | (vd.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	void opIVX(Bit32 baseValue, Bit1 vm, Bit5 vs2, Bit5 rs1, Bit5 vd)
	{
		/*
		    31 .. 26 | 25 | 24 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			  func6    vm      vs2       rs1        func3       vd     opcode

			func6, func3, and opcode must be encoded in the baseValue
		*/
		uint32_t v = (vm.v<<25) | (vs2.v<<20) | (rs1.v<<15) | (vd.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	void opFVF(Bit32 baseValue, Bit1 vm, Bit5 vs2, Bit5 rs1, Bit5 vd)
	{
		/*
		    31 .. 26 | 25 | 24 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			  func6    vm      vs2       rs1        func3       vd     opcode

			func6, func3, and opcode must be encoded in the baseValue
		*/
		uint32_t v = (vm.v<<25) | (vs2.v<<20) | (rs1.v<<15) | (vd.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	void opMVX(Bit32 baseValue, Bit1 vm, Bit5 vs2, Bit5 rs1, Bit5 d)
	{
		/*
		    31 .. 26 | 25 | 24 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			  func6    vm      vs2       rs1        func3     vd/rd    opcode

			func6, func3, and opcode must be encoded in the baseValue
		*/
		uint32_t v = (vm.v<<25) | (vs2.v<<20) | (rs1.v<<15) | (d.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	
	void opCSR(Bit32 baseValue, Bit12 csr, Bit5 rs1_uimm, Bit5 rd)
	{
		/*
		    31 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			   csr     rs1_uimm     func3       rd     opcode

			func3 and opcode must be encoded in the baseValue
		*/
		uint32_t v = (csr.v<<20) | (rs1_uimm.v<<15) | (rd.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	
	void opR4(Bit32 baseValue, Bit5 rs3, Bit5 rs2, Bit5 rs1, Bit3 rm, Bit5 rd)
	{
		/*
			31 .. 27 | 26 .. 25 | 24 .. 20 | 19 .. 15 | 14 .. 12 | 11 .. 7 | 6 .. 0
			   rs3        fmt        rs2        rs1        rm         rd      opcode

			fmt and opcode must be encoded in the baseValue
		*/
		uint32_t v = (rs3.v<<27) | (rs2.v<<20) | (rs1.v<<15) | (rm.v<<12) | (rd.v<<7);
		v |= baseValue.v; // force-encode base value
		append4B(v);
	}
	bool isValiCidx(uint32_t idx) const { return 8 <= idx && idx < 16; }

public:
	void L(Label& label) { labelMgr_.defineClabel(label); }
	Label L() { Label label; L(label); return label; }
	/*
		assign src to dst
		require
		dst : does not used by L()
		src : used by L()
	*/
	void assignL(Label& dst, const Label& src) { labelMgr_.assign(dst, src); }
	/*
		put the absolute address of label to buffer
		@note the put size is 4(32-bit), 8(64-bit)
	*/
	void putL(const Label &label)
	{
		Jmp jmp(getCurr());
		opJmp(label, jmp);
	}

	// constructor
	CodeGenerator()
	{
		labelMgr_.set(this);
	}
	void reset()
	{
		ClearError();
		resetSize();
		labelMgr_.reset();
		labelMgr_.set(this);
	}

	bool hasUndefinedLabel() const { return labelMgr_.hasUndefClabel(); }
};
#include "xbyak_riscv_mnemonic.hpp"

} // Xbyak_riscv

