CREATE OR REPLACE PACKAGE dwh_extract_pips
AS
--
-- NAME:  dwh_extract_pips 
-- TYPE:  PL/SQL Package specification
-- TITLE: Package holding plsql supporting extraction of pips partner data and  
--      : invoice data to dwh
-- NOTES:
--
--$Revision:   1.1  $
-------------------------------------------------------------------------------
-- Version | Date     | Author        | Reason
-------------------------------------------------------------------------------
-- 1.0     |24/02/2014| L. Holton     | Original Version
-- 1.1     |22/05/2014| R.Jonnalagadda| Added New procedure for trade_partner_cntry
--         |          |               | and also new input parameter feed id for
--         |          |               | the dimension procedures 
-------------------------------------------------------------------------------

    --
    -- UNIT:        p_extract_trade_partner
    -- DESCRIPTION: This procedures extracts partner account 
    -- USAGE:       dwh_extract_pips.p_extract_trade_partner (L_feed_id)
    -- PARAMS:      I_feed_id - Feed number passed in by GDI process
    -- NOTES:
    --
    PROCEDURE p_extract_trade_partner(I_feed_id  IN s_datatype.feed_id);

    --
    -- UNIT:        p_extract_trade_ptnr_cntry
    -- DESCRIPTION: This procedures extracts partner country data
    -- USAGE:       dwh_extract_pips.p_extract_trade_ptnr_cntry (L_feed_id)
    -- PARAMS:      I_feed_id - Feed number passed in by GDI process
    -- NOTES:
    --
    PROCEDURE p_extract_trade_ptnr_cntry(I_feed_id IN s_datatype.feed_id);
    --
    -- UNIT:        p_extract_invoice
    -- DESCRIPTION: This extracts invoice header and detail data
    -- USAGE:       dwh_extract_pips.p_extract_invoice
    -- PARAMS:      None
    -- NOTES:

    PROCEDURE p_extract_invoice;


END dwh_extract_pips;
/
SHOW ERRORS
