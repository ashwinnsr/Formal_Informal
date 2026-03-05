# VERIFIED ITEM CODES - From Official Codebooks

## ASI 2023-24 (Confirmed from Codebooks)

### Block F - Outsourcing
- **Item 7 (F7)**: "Cost of contract and commission work done by others"
- **Source**: ASI schedule_2023-24.pdf, Block F, Page 6
- **Our test**: F7 has 2,671 non-zero values ✅

### Block E - Employment  
- **Column 5 (EI7)**: Total Mandays Worked
- **Source**: Tabulation_Programme_ASI_23-24.pdf, Page 1
- **Our test**: EI7 has 367,960 non-zero values ✅
- **Calculation**: Sum EI7 across all worker categories, then divide by 300

### Block J - Output/GVA
- **Need to identify** which J columns contain GVA and Total Output
- **Action**: Check schedule for specific column definitions

---

## ASUSE 2023-24 (Confirmed from Codebooks)

### Block 2 - Job Work Definition

**⚠️ CRITICAL DECISION NEEDED:**

The codebook says:
- **nature_of_operation = 3**: "Working on contract/piece rate basis"
- This is the **official job work definition**

BUT our test showed:
- **nature_of_operation = 3**: Only **30 textile/apparel enterprises**
- **contract_manuf_service = 1**: **7,519 textile/apparel enterprises**

**Question for you**: Which definition should we use?

#### Option A: Codebook Definition (Narrow)
```r
filter(nature_of_operation == 3)
```
- ✅ Theoretically precise
- ❌ Only 30 enterprises (statistically unreliable)
- ❌ Less than 1 per state-industry on average

#### Option B: Contract Manufacturing (Broad)
```r
filter(contract_manuf_service == 1)  
```
- ✅ 7,519 enterprises (robust sample)
- ✅ ~200-300 per state-industry
- ⚠️ Broader definition (includes all contract manufacturing)
- ⚠️ Need to justify in dissertation methodology

**My recommendation**: Use **Option B** (`contract_manuf_service = 1`) for statistical power, but document this choice and explain that it captures the broader category of contract manufacturing, which includes job work.

---

### Block 5 - Wages
- **Item 1301**: "Salary, wages and allowances to hired workers"
- **Source**: Layout Excel, Block 5
- **Variable**: Need to filter for `item_no == 1301` (or similar code)

### Block 4 - Hired Workers
- **Variables**: `hired_male_avg` + `hired_female_avg` (or total)
- **Source**: Layout Excel, Level 04
- **Codes**: Items 1201 (male) + 1202 (female) + 1203 (transgender)

---

## Next Step: Verify Block Structure

Before building the final script, I need to check:

1. **Are Block 4 variables in a separate file?** (Our inspection only looked at Blocks 1, 2, 5)
2. **What are the exact column names in Block 5?** (is it `item_no` or `item_1301`?)
3. **Which J columns have GVA and Output in Block J?**

**Should I**:
- A) Create analysis script with educated guesses for these remaining items
- B) Create a quick test script to check Block 4 structure first
- C) You tell me the specific column names from the layout file
