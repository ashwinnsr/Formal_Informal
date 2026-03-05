# CRITICAL CORRECTIONS TO USER-PROVIDED SCRIPT

## ❌ Issues with the Provided "Master Script"

The script you provided has several assumptions that don't match your actual data:

###  1. File Format Mismatch
**Script assumes**: `.dta` files (Stata format)
```r
asi_a <- read_dta("Data/ASI/ASI_Block_A.dta")
```

**Your actual files**: `.sav` files (SPSS format)
```r
asi_a <- read_sav("ASI_Data/ASI_202324sav/blkA202324.sav")
```

### 2. ASUSE Block Structure Wrong
**Script assumes**: Wide format with separate columns
```r
asuse_labor <- asuse_4 %>%
  transmute(
    Hired_Workers = rowSums(select(., matches("1201|1202|1203")))
  )
```

**Your actual data**: LONG format with `item_no` column
```r
# Block 4 has: item_no, value_rs (just like Block 5!)
# Need to filter for correct item code and reshape
```

### 3. Item Codes Don't Exist
**Script assumes**: Item 1301 for wages

**Your actual data**: Item 1301 doesn't exist!
- Item codes are in 400-600 range
- Need to identify correct codes from actual data

### 4. Column Names Don't Match
**Script uses**: `col5`, `Value`, `DSL`, etc.

**Your actual columns**: `EI7`, `value_rs`, `a1`/`AE01`/`AF01`, etc.

---

## ✅ What We Know For Sure

### ASI (Verified)
- **Factory ID**: Changes by block (`a1`, `AE01`, `AF01`, `AJ01`)
- **Employment**: Sum `EI7` across all worker types (Item code `EI1`)
- **Outsourcing**: `F7` column in Block F
- **NIC code**: `a5` in Block A
- **Multiplier**: `mult` in Block A

### ASUSE (Verified)
- **Enterprise ID**: Combination of `fsu_serial_no` + `sample_est_no`
- **Job work filter**: `contract_manuf_service == "1"` (7,519 enterprises)
- **NIC code**: `major_nic_5dig` in Block 2
- **Multiplier**: `mlt`
- **Blocks 4 & 5 structure**: BOTH are long format with `item_no` and `value_rs`

### Still Need to Find
1. **ASUSE worker count item code** (Block 4)
2. **ASUSE wage item code** (Block 5)  
3. **ASI Block J columns** for GVA and Output

---

## 🔍 Next Step: Run Item Code Finder

```r
source("Find_ASUSE_Item_Codes.r")
```

This will:
1. Show all item codes in Blocks 4 and 5
2. Filter to textile/apparel job work enterprises
3. Show which item codes are most common
4. Help identify worker counts vs wages

**Expected output**:
- Worker counts: Item code with values like 1-50 (small integers)
- Wages: Item code with values like 10,000-500,000 (large rupees)

Once we have these, I'll create the CORRECT final script!
