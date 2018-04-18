CREATE OR REPLACE PACKAGE fra_invoice
AS
--
-- NAME:  fra_invoice 
-- TYPE:  PL/SQL Package specification
-- TITLE: Package holding plsql supporting creation of franchise and wholesale 
--      : invoices
-- NOTES:
--
--$Revision:   1.9  $
-------------------------------------------------------------------------------
-- Version | Date     | Author        | Reason
-------------------------------------------------------------------------------
-- 1.0     |23/01/2013| L. Holton     | Original Version
-- 1.1     |31/01/2013| L. Holton     | Add function to return long string
--         |          |               | of countries of origin for an invoice
-- 1.2     |22/02/2013| L. Holton     | Add spreadsheet upload procedure
-- 1.3     |15/03/2013| L. Holton     | Add procedure to autoallocate payment to
--         |          |               | invoices
-- 1.4     |16/04/2013| L. Holton     | Parameterise autoallocate payment to
--         |          |               | allow for user selection of invoices
--         |          |               | to pay.
-- 1.5     |19/04/2013| L. Holton     | Add procedures to upload and populate 
--         |          |               | invoice tables to produce non merch 
--         |          |               | invoices.
-- 1.6     |16/05/2013| L.Holton      | Add procedure p_apply_trans to allocate
--         |          |               | credits against invoices
-- 1.7     |19/09/2013| L.Holton      | Add abiltiy to add consignments
--         |          |               | to an invoice
-- 1.8     |07/10/2013| L.Holton      | Add filename as paramter to 
--         |          |               | p_create_non_merch_invoice
-- 1.9     |01/06/2015| M.Cackett     | Added function f_country_list_smry
--         |          |               | Used by new Summary Invoice report.
--         |18/04/2018| L.Holton      | Exciting stuff to test GIT



    --
    -- UNIT:        p_create_invoice
    -- DESCRIPTION: Create invoice from passed consignment numbers
    -- USAGE:       fra_invoice.p_create_invoice (I_consignment_list
    --      :                                          => L_consignment_list,
    --      :                                     I_user
    --      :                                         => L_user)
    --
    -- PARAMS:      I_consignment_list - list of colon delimited consignment
    --                                    numbers
    --              I_user - user name of person creating invoice
    -- NOTES:
    PROCEDURE p_create_invoice (I_consignment_list IN s_datatype.long_string,
                                I_user IN s_datatype.user_id,
				IO_invoice_header_id  IN OUT 
				    fra_invoice_header.invoice_header_id%TYPE);


    --
    -- UNIT:        p_apply_uplift
    -- DESCRIPTION: Applies the uplift to the passed invoice, this can be one or 
    --            : more amounts based on cost and/or retail prices.
    -- USAGE      : p_apply_uplift (I_invoice_header_id 
    --            :                      => L_invoice_header_id,
    --            :                 I_user
    --            :                     => L_user)
    -- PARAMS     : I_invoice_header_id - unique identifier for a invoice
    --            : I_user - username of person applying the uplift
    -- NOTES:     :
    PROCEDURE p_apply_uplift (I_invoice_header_id IN 
				  fra_invoice_header.invoice_header_id%TYPE,
                              I_user IN s_datatype.user_id);
			       


    --
    -- UNIT:        f_country_list
    -- DESCRIPTION: Returns a string containing all the 
    --            : countries of the suppliers in an invoice
    -- USAGE:       L_country_list := 
    --                  fra_invoice.f_country_list (L_invoice_header_id)
    -- PARAMS:      I_invoice_header_id invoice unique identifier
    -- RETURNS:     L_counry_list  country names of items in invoice
    -- NOTES:
    --
    FUNCTION f_country_list (I_invoice_header_id IN 
                                 fra_invoice_header.invoice_header_id%TYPE)
    RETURN VARCHAR2;

    
    --
    -- UNIT:        f_country_list_smry
    -- DESCRIPTION: Returns a string containing all the 
    --            : countries of the suppliers in a summary invoice
    -- USAGE:       L_country_list := 
    --                  fra_invoice.f_country_list (L_summary_invoice_id)
    -- PARAMS:      I_summary_invoice_id smry invoice unique identifier
    -- RETURNS:     L_counry_list  country names of items in invoice
    -- NOTES:
    --
    FUNCTION f_country_list_smry (I_summary_invoice_id IN 
                                 pips_multi_invoice_smry.summary_invoice_id%TYPE)
    RETURN VARCHAR2;

    
    
    --
    -- UNIT:        p_upload_spreadsheet
    -- DESCRIPTION: Moves blob data from apex table wwv_flow_file_objects$ to
    --            : the working table w_fra_partner_prd_po. This supplies the
    --            : partners po number(s) and products with their translations
    --            : to generate the invoices for the current consignments.
    -- USAGE      : p_upload_spreadsheet (I_partner_id 
    --            :                          => L_partner_id,
    --            :                       I_blob_name
    --            :                           => L_blob_name,
    --            :                       I_user
    --            :                            => L_user)
    -- PARAMS     : I_partner_id - unique identifier for partner spreadsheet
    --            :                relates too 
    --            : I_blob_name - name of spreasheet uploaded via apex
    --            : I_user - username of person uploading the spreadsheet
    -- NOTES:     :
    PROCEDURE p_upload_spreadsheet (I_partner_id IN 
				        fra_partner.partner_id%TYPE,
				    I_blob_name IN VARCHAR2, 
                                    I_user IN s_datatype.user_id);

    --
    -- UNIT       : p_apply_payment
    -- DESCRIPTION: Takes the payment and applies to invoices starting from 
    --            : from the oldest that isnt fully paid
    -- USAGE      : fra_invoice.p_apply_payment (I_payment_id => L_payment_id,
    --            :                              I_user => L_user,
    --            :                              I_invoice_list => L_invoice_list);
    -- PARAMS     : I_payment_id  - ID of payment made
    --            : I_user        - username of person applying the payment
    --            : I_invoice_list - List of invoices to apply payment to
    -- NOTES      :
    PROCEDURE p_apply_payment (I_payment_id IN fra_payment.payment_id%TYPE,
	                        I_user IN s_datatype.user_id,
                                I_invoice_list IN s_datatype.long_string
                                    DEFAULT NULL);


    --
    -- UNIT       : p_apply_trans
    -- DESCRIPTION: Takes the transaction (credit) and applies to invoices starting from 
    --            : from the oldest that isnt fully paid
    -- USAGE      : fra_invoice.p_apply_trans (I_transaction_id => L_transaction_id,
    --            :                              I_user => L_user,
    --            :                              I_invoice_list => L_invoice_list);
    -- PARAMS     : I_trans_id  - ID of transaction made (credit)
    --            : I_user        - username of person applying the payment
    --            : I_trans_list - List of invoices to apply credit to
    -- NOTES      :
    PROCEDURE p_apply_trans (I_transaction_id IN fra_partner_trans.transaction_id%TYPE,
	                        I_user IN s_datatype.user_id,
                                I_invoice_list IN s_datatype.long_string
                                    DEFAULT NULL);

    --
    -- UNIT:        p_upload_non_merch
    -- DESCRIPTION: Moves blob data from apex table wwv_flow_file_objects$ to
    --            : the working table w_fra_non_merch_invoice. This is used
    --            : to populate fra_invoice_header and fra_invoice_line to 
    --            : produce the invoices for non merchendise goods.
    -- USAGE      : p_upload_non_merch (I_partner_id 
    --            :                          => L_partner_id,
    --            :                       I_blob_name
    --            :                           => L_blob_name,
    --            :                       I_user
    --            :                            => L_user)
    -- PARAMS     : I_partner_id - unique identifier for partner spreadsheet
    --            :                relates too 
    --            : I_blob_name - name of spreasheet uploaded via apex
    --            : I_user - username of person uploading the spreadsheet
    -- NOTES:     :
    PROCEDURE p_upload_non_merch (I_partner_id IN 
				        fra_partner.partner_id%TYPE,
				    I_blob_name IN VARCHAR2, 
                                    I_user IN s_datatype.user_id);

    --
    -- UNIT:        p_create_non_merch_invoice
    -- DESCRIPTION: Create invoice for passed partner from uploaded non
    --             : merch invoice data
    -- USAGE:       fra_invoice.p_create_non_merch_invoice (I_partner_id
    --      :                                                  => L_consignment_list,
    --      :                                              I_user
    --      :                                                  => L_user,
    --      :                                              I_filename
    --      :                                                  => L_filename,
    --      :                                              O_invoice_header_id
    --      :                                                  => L_invoice_header_id)
    --
    -- PARAMS:      I_partner_id - partner id for created invoice
    --              I_user - user name of person creating invoice
    --              I_filename - file being processed
    --              O_invoice_header_id - primary key of index created 
    -- NOTES:
    PROCEDURE p_create_non_merch_invoice (I_partner_id IN fra_partner.partner_id%TYPE,
                                          I_user IN s_datatype.user_id,
                                          I_filename IN w_fra_non_merch_invoice.file_name%TYPE,
				          O_invoice_header_id OUT 
				              fra_invoice_header.invoice_header_id%TYPE);


END fra_invoice;
/
SHOW ERRORS

