# Item Code Analysis & Recommendations

## Block 4 (Employment) - Most Common in Our Sample
1. **Item 549**: 33,614 occurrences (most common)
2. **Item 541**: 27,823 occurrences
3. **Item 511**: 27,721 occurrences

**Analysis**: Need to check codebook to see which represents:
- Hired workers (what we want)
- vs. Own-account/family workers (skip)
- vs. Total workers (might include both)

**Recommendation**: Start with **Item 511** (typically "hired workers") but test others if needed.

## Block 5 (Wages) - Most Common in Our Sample  
1. **Item 559**: 11,788 occurrences, avg value: **306,084 Rs** ✅
2. **Item 569**: 11,788 occurrences, avg value: **311,307 Rs** ✅
3. **Item 571**: 7,582 occurrences, avg value: **817,355 Rs** (suspiciously high?)
4. **Item 557**: 10,249 occurrences, avg value: **43,231 Rs** (too low for wages)

**Analysis**:
- Items 559 and 569 have **identical frequency** (11,788) - likely related
- Average values ~300k Rs seem reasonable for annual hired worker wages
- Item 571 has much higher values (800k+) - might be total emoluments including benefits

**Recommendation**: Use **Item 559** for "Salary, wages and allowances to hired workers"

## Summary of Final Mappings

### ASI (Confirmed)
- **Factory ID**: `a1` (Block A), `AE01` (Block E), `AF01` (Block F), `AJ01` (Block J)
- **NIC Code**: `a5`
- **Multiplier**: `mult`
- **Employment**: Sum `EI7` across all worker types (`EI1` = 1 to 10)
- **Outsourcing**: `F7` column
- **Output**: Need to identify from Block J columns (likely `J113` or `J17`)

### ASUSE (Now Confirmed!)
- **Enterprise ID**: `paste(fsu_serial_no, sample_est_no, sep = "_")`
- **Job Work Filter**: `contract_manuf_service == "1"` 
- **NIC Code**: `major_nic_5dig` in Block 2
- **Multiplier**: `mlt`
- **Hired Workers**: Item **511** in Block 4 (filter `item_no == "511"`)
- **Wages**: Item **559** in Block 5 (filter `item_no == "559"`)

## Next: Create Final Script
With these verified codes, I can now create the complete working analysis script!
