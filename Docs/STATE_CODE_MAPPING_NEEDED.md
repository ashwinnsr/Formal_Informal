# CRITICAL: State Code Mapping Between ASI and ASUSE

## The Issue

**ASI state codes** (from factory ID): 10, 11, 12, 13, 14, 15, 16, 20, 21

**ASUSE district codes**: 01, 02, 03... 75

**Problem**: These use DIFFERENT coding systems!
- ASI uses 2-digit state codes
- ASUSE uses 2-digit district codes (NOT directly state codes)

## What You Need to Do

### Option 1: Map ASUSE Districts to States (Recommended)

Create a mapping table from ASUSE district codes to ASI state codes.

**Steps**:
1. Check ASUSE codebook to see which districts belong to which states
2. Create a lookup table:

```r
# Example mapping (VERIFY with your codebooks!)
district_to_state <- data.frame(
  ASUSE_district = c("01", "02", "03", "04", "05", ...),
  ASI_state = c("10", "10", "11", "11", "12", ...)
)
```

3. Join this to ASUSE data before aggregating

### Option 2: Use NSS Region

Check if ASUSE has a `nss_region` variable that maps to states.

From your test, ASUSE has 87 NSS regions. The first 2 digits of NSS region typically indicate state.

```r
asuse_firm_level <- asuse_firm_level %>%
  mutate(
    State_Code = substr(as.character(nss_region), 1, 2)
  )
```

### Option 3: Match State Names

Get state names from both datasets and match by name:

1. Map ASI state codes (10, 11, 12...) to state names using codelist PDF
2. Extract state info from ASUSE 
3. Join by state name

## Quick Test to Run

```r
# Check if ASUSE has nss_region that could work
asuse_2 <- read_sav("ASUSE_Data/ASUSE202324sav/LEVEL - 02(Block 2).sav")

# Check nss_region first 2 digits
asuse_2 <- asuse_2 %>%
  mutate(state_from_nss = substr(as.character(nss_region), 1, 2))

table(asuse_2$state_from_nss)
```

If this gives you ~9-10 unique values matching ASI's state range, **USE THIS**!

## Recommended Action

1. **Run the nss_region test above**
2. **If it works**: Update `Stage1_StateIndustry_Analysis.r` line ~160:
   ```r
   State_Code = substr(as.character(nss_region), 1, 2)
   ```
3. **If it doesn't work**: Create manual district-to-state lookup table from codebooks

## Where to Fix in the Script

In `Stage1_StateIndustry_Analysis.r`, around line 160-165:

```r
# Currently placeholder:
asuse_firm_level <- asuse_firm_level %>%
  mutate(
    State_Code = State_Code_ASUSE  # NEEDS FIXING!
  )

# Replace with one of:
# Option A: NSS region
  mutate(State_Code = substr(as.character(nss_region), 1, 2))

# Option B: Manual mapping
  left_join(district_to_state, by = c("State_Code_ASUSE" = "ASUSE_district")) %>%
  mutate(State_Code = ASI_state)
```

This is the LAST piece needed to make the analysis work!
