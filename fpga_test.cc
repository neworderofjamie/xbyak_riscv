// Standard C++ includes
#include <fstream>
#include <iostream>

// Standard C includes
#include <cassert>
#include <cstring>

// POSIC includes
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/mman.h>

//
#include "xbyak_riscv.hpp"
//#include "xbyak_riscv_mnemonic.hpp"

#define INSTRUCTION_SIZE 32764
#define DATA_SIZE 65536
#define GPIO_SIZE 8192

/*struct Code : Xbyak_riscv::CodeGenerator {
    Code()
    {
        using namespace Xbyak_riscv;
        
        // x1 (sum) = 0
        addi(x1, x0, 0);
        
        // x2 (address) = 0
        addi(x2, x0, 0);
        
        // x3 (count) = 2000
        addi(x3, x0, 2000);

        Label loop;
        L(loop);
        
        // x4 = *x2 (address)
        lw(x4, x2, 0);
        
        // x1 (sum) += x4
        add(x1, x1, x4);
        
        // x2 (address) += 4
        addi(x2, x2, 4);
        
        // While x2 (address) < x3 (count), goto loop
        blt(x2, x3, loop);
        
        // *x2 address = x1 (sum)
        sw(x1, x2, 0);  
    }
};*/

struct Code : Xbyak_riscv::CodeGenerator {
    Code()
    {
        using namespace Xbyak_riscv;
        
        // alpha = e^(-1/20)
        vlui(v0, 7792);
        
        // v = 0
        vlui(v1, 0);
        
        // v_thresh = 1
        vlui(v2, 8192);

        // v_reset = 0
        vlui(v3, 0);
        
        // i = vmem[0..32]
        vloadv(v4, zero, 0);
        
        // t = 6400
        addi(x1, zero, 25);
        slli(x1, x1, 8);
        
        // a = 64 (2 bytes * 32 lanes)
        addi(x2, zero, 64);
        
        Label loop;
        L(loop);
        
        // v *= alpha
        vmul(13, v1, v1, v0);
        
        // v += i
        vadd(v1, v1, v4);
        
        // spk = v > 1.0
        vslt(x3, v2, v1);
        
        // v = spk ? v_reset : v
        vsel(v1, x3, v3);
        
        //vmem[a...a+32] = v
        vstore(v1, x2);
        
        // a += 64 (2 bytes * 32 lanes)
        addi(x2, x2, 64);
        
        // While x2 (address) < x1 (count), goto loop
        blt(x2, x1, loop);
    }
};

int main(int argc, char** argv)
{
    Code code;

    std::ofstream fout;
    fout.open("file.bin", std::ios::binary);

    fout.write(reinterpret_cast<const char*>(code.getCode().data()), code.getCode().size() * 4);
    
    return 0;
    // Open memory device
    // **NOTE** O_SYNC turns of caching
    int memFD = open("/dev/mem", O_RDWR | O_SYNC);
    if(memFD == -1) {
        std::cerr << "/dev/mem open failure (" << errno<< " = " << strerror(errno) << ")" << std::endl;
        return 1;
    }
    uint32_t *itcm = reinterpret_cast<uint32_t*>(mmap(NULL, INSTRUCTION_SIZE, PROT_WRITE, MAP_SHARED, memFD,
                                                      0xA4000000));
    if(itcm == MAP_FAILED) {
        std::cerr << "ITCM map failed (" << errno << " = " << strerror(errno) << ")" << std::endl;
        return 1;
    }                                      
    uint32_t *dtcm = reinterpret_cast<uint32_t*>(mmap(NULL, DATA_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, memFD,
                                                      0xA7000000));
    if(dtcm == MAP_FAILED) {
         std::cerr << "DTCM map failed (" << errno << " = " << strerror(errno) << ")" << std::endl;
         return 1;
    }             
    uint32_t *gpio = reinterpret_cast<uint32_t*>(mmap(NULL, GPIO_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, memFD,
                                                      0xA5000000));
    if(gpio == MAP_FAILED) {
         std::cerr << "GPIO map failed (" << errno << " = " << strerror(errno) << ")" << std::endl;
         return 1;
    }                     

    // Disable
    gpio[0] = 0x0;

    // Copy data to DTCM
    for(unsigned int i = 0; i < 500; i++) {
        dtcm[i] = i;
    }
    dtcm[500] = 0;
  
    
    // Copy code to ITCM
    std::copy(code.getCode().cbegin(), code.getCode().cend(), itcm);
       
    // Enable
    gpio[0] = 0xFFFFFFFF;
    
    // Sleep for 1 second to wait for completion
    sleep(1);
    
    for(unsigned int i = 0; i < 501; i++) {
        printf("%u,", dtcm[i]);
    }
    
    
    // Unmap memory
    munmap(gpio, GPIO_SIZE);
    munmap(dtcm, DATA_SIZE);
    munmap(itcm, INSTRUCTION_SIZE);
    

    // Close memory device
    close(memFD);

}
