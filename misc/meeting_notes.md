
## Meeting notes

  - Company helps appeal assessment values for residential and
    commercial
  - Both residential and commercial are important to the model, although
    the majority of company’s profits come from corporate clients
  - Tax classes (classified by government, with 01 residential, 05 light
    industrial, 06 commercial being the importnat classes)
  - Some condos + commercial complexes have split classes
  - Mill rate is used interchangeably with tax rate, and is unique to a
    specific code (called mill rate because its over 1000)
  - eg: “12.3\* assessment /1000” mill rate is 12.3
  - Three things he is interested in primarily:
    1.  Predicting mill rate (published in April but assessments
        published in January) which changes every year, during the blank
        period from jan-april
    2.  Predicting next years assessment values (once mill rates are
        known, for the period between May-Dec)
    3.  Predicting final tax that will be owed
  - Their service is used to help with tax budgeting
  - Sometimes values are missing, but we still need to predict these
    things given what we do have available for a given property
  - Municipal government has budget, and determines mill rates for each
    tax code in order to raise the budget amount (from other funds also,
    but estimated about 90% of fund raised by property taxes)
  - Mentioned it would be interesting to quantify relationship between
    mill rates and assessment values, but accurate prediction is
    priority
  - Roll number uniquely identifies properties across datasets (see
    handout for the form of this identifier)
  - 2016-2020 data is available (2015?)
  - Vancouver municipality is a special case when it comes to tax coding
    - this should be treated as such and is not comparable to other
    municipalities
  - Sales data is available (they bought this from government); each row
    is a sales transaction (filter to market sales only, to leave name
    transfers out of it)
  - Currently if property assessments go up 10%, they are assuming mill
    rate goes down 6-7% due to inflation and larger budgets and
    predicting like this (it’s not working well)
  - Assumes large chunk of variability in mill rate is due to assessment
    (and not just municipal budget amount)
  - Table handout is for changes in assessment values (are they
    averages? he’s not sure but we could check)
  - Exemption exists but is maybe not that important
  - Whatever information is used for assessment by the government is
    also available to us
