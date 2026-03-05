# Data Mapping Summary - Ready for Final Analysis Script

## Test Results - Confirmed Working Numbers

### ASI (Formal Sector)
- **Total textile/apparel factories**: 8,137
  - Textiles (NIC 13): 4,692
  - Apparel (NIC 14): 3,445

### ASUSE (Informal Sector)  
- **Total textile/apparel enterprises**: 67,052
- **Job work enterprises** (contract_manuf_service = 1): **7,519** ✅
  - This gives ~200-300 per state-industry

### Geographic Level Decision
**Use STATE-INDUSTRY aggregation** (not district) because geographic codes are suppressed

---

## Variable Mappings - Confirmed

### ASI Block A (Identification)
```r
Factory_ID = a1
NIC_Code = a5
Multiplier = mult
# Note: State/district suppressed - will need alternative approach
```

### ASI Block E (Employment)
**Structure**: Long format - 10 rows per factory

```r
Factory_ID = AE01
Worker_Category = EI1  # Values 1-10
Mandays_Male = EI3     # 362,934 non-zero values
Mandays_Female = EI4   # 19,614 non-zero values  
Mandays_Total = EI5    # 367,953 non-zero values
Workers = EI6          # 377,426 non-zero values
Mandays_Worked = EI7   # 367,960 non-zero values
Wages = EI8            # Wage/emolument value
```

**To calculate total employment**: Sum EI7 (Mandays_Worked) across all worker categories, then divide by 300

### ASI Block F (Expenses)
**Structure**: One row per factory

```r
Factory_ID = AF01
# Need to verify from codebook which F column is contract work
# Best candidates based on prevalence:
#   F5: 49,045 non-zero (most likely - common expense)
#   F6: 10,474 non-zero (alternative - less common, might be contract-specific)
```

**ACTION NEEDED**: Check `ASI_Data/ASI_Info/f.Codelist24 (2).pdf` to identify which F column represents "Cost of contract and commission work done by others"

### ASI Block J (Output/GVA)
**Structure**: Long format - multiple rows per factory

```r
Factory_ID = AJ01
Item_Code = J11
Product_Code = J13
# Need to identify which J columns are GVA and Total_Output
```

**ACTION NEEDED**: Check codebook to find GVA and Output columns

---

### ASUSE Block 2 (Characteristics)
**Structure**: One row per enterprise

```r
Enterprise_ID = paste(fsu_serial_no, sample_est_no, sep="_")
District = district
NIC_2digit = substr(major_nic_5dig, 1, 2)
NIC_5digit = major_nic_5dig
Job_Work_Flag = contract_manuf_service  # Use value "1" for job work
Multiplier = mlt
```

### ASUSE Block 5 (Wages/Expenses)
**Structure**: Long format with item codes

```r
Enterprise_ID = paste(fsu_serial_no, sample_est_no, sep="_")
Item_Code = item_no
Value = value_rs
Multiplier = mlt
```

**Most common item codes** (from test):
- 569: 141,763 occurrences
- 559: 141,744 occurrences  
- 589: 119,470 occurrences
- 579: 119,433 occurrences
- 571: 118,554 occurrences

**ACTION NEEDED**: Check `ASUSE_Data/ASUSE_Info/NSS_ASUSE_23_24_Layout_mult_post (2)(3).xlsx` to find:
1. Item code for "Hired worker emoluments/wages"
2. Item code for "Number of hired workers"

**Note**: These might be in items 550-599 range (operating expenses)

---

## Critical Decisions Made

1. ✅ **Geographic aggregation**: STATE-INDUSTRY (not district-industry)
2. ✅ **Job work definition**: `contract_manuf_service = 1` (gives 7,519 enterprises)
3. ✅ **Employment measure**: Sum EI7 across worker categories ÷ 300
4. ⏳ **Outsourcing expense**: Pending codebook verification (likely F5 or F6)
5. ⏳ **ASUSE wage items**: Pending codebook verification

---

## Next Steps

### Option A: Proceed with Best Guesses
If you want to move forward quickly, I can create the analysis script with educated guesses:
- Use F5 for contract work (most common expense after F3/materials)
- Use item codes 550-599 range for wages (typical operating expense range)
- You can refine later after checking codebooks

### Option B: Verify Codebooks First (Recommended)
1. Open `ASI_Data/ASI_Info/f.Codelist24 (2).pdf`
2. Look for entry describing "Contract and commission work" or "Work done by others"
3. Note which F column number it corresponds to

4. Open `ASUSE_Data/ASUSE_Info/NSS_ASUSE_23_24_Layout_mult_post (2)(3).xlsx`
5. Find Block 5 sheet
6. Look for item codes related to:
   - "Emoluments to hired workers"
   - "Number of hired workers"

Tell me which option you prefer, or just tell me what you find in the codebooks!

---

## What's Ready

Once we have the item codes, I can immediately create:
- Updated `Stage1_Analysis.r` with all correct mappings
- State-industry aggregation logic
- Proper reshaping for long-format blocks
- All three regression models
- Full documentation

**Estimated time to create final script once we have the codes**: 15-20 minutes
