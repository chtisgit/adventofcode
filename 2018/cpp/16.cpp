#include <algorithm>
#include <array>
#include <cstdio>
#include <set>
#include <stdexcept>
#include <vector>

struct Op {
	int code, A, B, C;
};

constexpr int INSTRUCTIONS = 16;
constexpr int REGS = 4;
struct CPU {
	using Instruction = void (CPU::*)(int, int, int);

	static void mustReg(std::initializer_list<int> L)
	{
		for (auto n : L)
			if (n < 0 || n >= REGS)
				throw std::out_of_range("not a valid register");
	}

	void addr(int, int, int);
	void addi(int, int, int);
	void mulr(int, int, int);
	void muli(int, int, int);
	void banr(int, int, int);
	void bani(int, int, int);
	void borr(int, int, int);
	void bori(int, int, int);
	void setr(int, int, int);
	void seti(int, int, int);
	void gtir(int, int, int);
	void gtri(int, int, int);
	void gtrr(int, int, int);
	void eqir(int, int, int);
	void eqri(int, int, int);
	void eqrr(int, int, int);

	bool operator==(const CPU &rhs) const
	{
		return reg == rhs.reg;
	}

	std::array<int, REGS> reg;
	std::vector<Instruction> instructions{
	    &CPU::addr, &CPU::addi, &CPU::mulr, &CPU::muli,
	    &CPU::banr, &CPU::bani, &CPU::borr, &CPU::bori,
	    &CPU::setr, &CPU::seti, &CPU::gtir, &CPU::gtri,
	    &CPU::gtrr, &CPU::eqir, &CPU::eqri, &CPU::eqrr};

	const char *instr(Instruction instr)
	{
		static std::vector<std::pair<CPU::Instruction, const char *>>
		    names = {{&CPU::addr, "addr"}, {&CPU::addi, "addi"},
			     {&CPU::mulr, "mulr"}, {&CPU::muli, "muli"},
			     {&CPU::banr, "banr"}, {&CPU::bani, "bani"},
			     {&CPU::borr, "borr"}, {&CPU::bori, "bori"},
			     {&CPU::setr, "setr"}, {&CPU::seti, "seti"},
			     {&CPU::gtir, "gtir"}, {&CPU::gtri, "gtri"},
			     {&CPU::gtrr, "gtrr"}, {&CPU::eqir, "eqir"},
			     {&CPU::eqri, "eqri"}, {&CPU::eqrr, "eqrr"}};
		auto it = std::find_if(
		    names.cbegin(), names.cend(),
		    [instr](const auto &p) { return p.first == instr; });
		if (it == names.cend())
			throw "fatal: no such instruction";
		return it->second;
	}

	void step(int op, int A, int B, int C)
	{
		if (op < 0 || op >= instructions.size())
			throw "fatal: op code out of range";
		(this->*(instructions[op]))(A, B, C);
	}

	void step(const Op &op)
	{
		step(op.code, op.A, op.B, op.C);
	}

	template<typename InputIterator>
	void run(InputIterator begin, InputIterator end)
	{
		std::for_each(begin, end, [this](const Op& op){
			this->step(op);
		});
	}

	std::set<int> findInstruction(const CPU &before, const Op &op) const
	{
		std::set<int> res;
		for (size_t i = 0; i < instructions.size(); i++) {
			try {
				CPU test = before;
				test.step(i, op.A, op.B, op.C);
				if (test == *this) {
					res.insert(i);
				}
			} catch (...) {
				// not our instruction
			}
		}
		return res;
	}
};

void CPU::addr(int A, int B, int C)
{
	mustReg({A, B, C});
	reg[C] = reg[A] + reg[B];
}
void CPU::addi(int A, int B, int C)
{
	mustReg({A, C});
	reg[C] = reg[A] + B;
}
void CPU::mulr(int A, int B, int C)
{
	mustReg({A, B, C});
	reg[C] = reg[A] * reg[B];
}
void CPU::muli(int A, int B, int C)
{
	mustReg({A, C});
	reg[C] = reg[A] * B;
}
void CPU::banr(int A, int B, int C)
{
	mustReg({A, B, C});
	reg[C] = reg[A] & reg[B];
}
void CPU::bani(int A, int B, int C)
{
	mustReg({A, C});
	reg[C] = reg[A] & B;
}
void CPU::borr(int A, int B, int C)
{
	mustReg({A, B, C});
	reg[C] = reg[A] | reg[B];
}
void CPU::bori(int A, int B, int C)
{
	mustReg({A, C});
	reg[C] = reg[A] | B;
}
void CPU::setr(int A, int B, int C)
{
	mustReg({A, C});
	reg[C] = reg[A];
}
void CPU::seti(int A, int B, int C)
{
	mustReg({C});
	reg[C] = A;
}
void CPU::gtir(int A, int B, int C)
{
	mustReg({B, C});
	reg[C] = A > reg[B] ? 1 : 0;
}
void CPU::gtri(int A, int B, int C)
{
	mustReg({A, C});
	reg[C] = reg[A] > B ? 1 : 0;
}
void CPU::gtrr(int A, int B, int C)
{
	mustReg({A, B, C});
	reg[C] = reg[A] > reg[B] ? 1 : 0;
}
void CPU::eqir(int A, int B, int C)
{
	mustReg({B, C});
	reg[C] = A == reg[B] ? 1 : 0;
}
void CPU::eqri(int A, int B, int C)
{
	mustReg({A, C});
	reg[C] = reg[A] == B ? 1 : 0;
}
void CPU::eqrr(int A, int B, int C)
{
	mustReg({A, B, C});
	reg[C] = reg[A] == reg[B] ? 1 : 0;
}

const std::set<int> &allInstructions()
{
	static std::set<int> all;
	if (all.size() == 0) {
		for (size_t i = 0; i < INSTRUCTIONS; i++) {
			all.insert(i);
		}
	}
	return all;
}

int main()
{
	std::vector<std::set<int>> instrperm(INSTRUCTIONS, allInstructions());
	std::vector<Op> program;
	int task1 = 0;

	for (int i = 1;; i++) {
		CPU before, after;
		Op op;

		if (scanf("Before: [%d, %d, %d, %d]\n", &before.reg[0],
			  &before.reg[1], &before.reg[2], &before.reg[3]) != 4)
			break;
		if (scanf("%d %d %d %d\n", &op.code, &op.A, &op.B, &op.C) != 4)
			break;
		if (scanf("After: [%d, %d, %d, %d]\n\n", &after.reg[0],
			  &after.reg[1], &after.reg[2], &after.reg[3]) != 4)
			break;

		auto &cur = instrperm[op.code];
		auto list = after.findInstruction(before, op);
		if (list.size() >= 3)
			task1++;

		std::set<int> oldset;
		oldset.swap(cur);
		std::set_intersection(oldset.cbegin(), oldset.cend(),
				      list.cbegin(), list.cend(),
				      std::inserter(cur, cur.begin()));
	}
	for(;;){
		Op op;
		if(scanf("%d %d %d %d\n", &op.code, &op.A, &op.B, &op.C) != 4) break;
		program.push_back(op);
	}

	printf("task1: %d\n", task1);

	for (size_t i = 0; i < instrperm.size(); i++) {
		std::set<int> alone;
		for (const auto &p : instrperm) {
			if (p.size() == 1)
				alone.insert(*p.begin());
		}
		for (auto &p : instrperm) {
			if (p.size() > 1) {
				for (auto code : alone) {
					p.erase(code);
				}
			}
		}
	}

	CPU cpu;
	std::fill(cpu.reg.begin(), cpu.reg.end(), 0);
	auto instrs = cpu.instructions;
	for(size_t i = 0; i < instrperm.size(); i++){
		if(instrperm[i].size() > 1){
			printf("error: ambiguous!");
			return 1;
		}
		//printf("opcode %d is this CPU's %d\n", i, *instrperm[i].begin());
		auto code = *instrperm[i].begin();
		cpu.instructions[i] = instrs[code];
	}

	cpu.run(program.cbegin(), program.cend());
	printf("task2: %d\n", cpu.reg[0]);

	return 0;
}
