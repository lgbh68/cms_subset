CREATE OR REPLACE PACKAGE BODY dwh_extract_pips
AS
--
-- NAME:  dwh_extract_pips 
-- TYPE:  PL/SQL Package 
-- TITLE: Package holding plsql supporting extraction of pips partner data and  
--      : invoice data to dwh
-- NOTES:
--
--$Revision:   1.2  $
-------------------------------------------------------------------------------
-- Version | Date     | Author        | Reason
-------------------------------------------------------------------------------
-- 1.0     |24/02/2014| L. Holton     | Original Version
-- 1.1     |13/05/2014| L. Holton     | default invoice level invoice type to
--         |          |               | partner where null
-- 1.2     |22/05/2014| R.Jonnalagadda| New procedure for trade_partner_cntry
--         |          |               | and modified to capture the changed
--         |          |               | records instead of max extract
-------------------------------------------------------------------------------

    --
    -- UNIT:        p_extract_trade_partner
    -- DESCRIPTION: This procedures extracts partner account 
    -- NOTES:
    --
    PROCEDURE p_extract_trade_partner (I_feed_id   IN s_datatype.feed_id)
    IS

        L_unit                    s_datatype.unit_name := 
                                     'dwh_extract_pips.p_extract_trade_partner';
        L_file_name               s_file_sequence.file_name%TYPE := NULL;
        L_select_string           s_datatype.long_query;

    BEGIN

        s_info.p_track_process (L_unit, s_const.C_begin, I_log_bool => TRUE);

        -- Get filename for trade partner
        -- 
        s_util.p_get_file_name(
                       I_file_id   =>  cms_file_const.C_dwh_trade_partner_file_id,
                       I_file_type =>  cms_file_const.C_dwh_file_type,
                       O_file_name =>  L_file_name);

        -- Generate sql string for trade_partner
        L_select_string := 
            'SELECT s_delim_str.f_get_string(wkq.key_data,1,'''                ||
                    cms_dwh_const.C_key_data_delim  || ''')  || '              ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.parent_partner_id|| '                                       ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.partner_code|| '                                            ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.partner_name|| '                                            ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.partner_type|| '                                            ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.partner_country_code|| '                                    ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.partner_invoice_type|| '                                    ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.contact_title|| '                                           ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.contact_first_name|| '                                      ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.contact_last_name|| '                                       ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.email_addr|| '                                              ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.payment_terms|| '                                           ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.cost_centre|| '                                             ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.gl_credit_code|| '                                          ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.gl_debit_code|| '                                           ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.vat_ind|| '                                                 ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.deld_outdir|| '                                             ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.int_orig_sys_bill_cust_ref|| '                              ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.int_orig_sys_bill_address_ref|| '                           ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.created_dtm|| '                                             ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.last_changed_dtm|| '                                        ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.created_by|| '                                              ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||tp.last_changed_by || '                                        ||
            ''''  ||cms_dwh_const.c_column_delimiter||''''                     ||
            ' ||  s_delim_str.f_get_string(wkq.key_data,2,'''                  ||
                    cms_dwh_const.C_key_data_delim  || ''')   '                ||
            ' FROM trade_partner tp, '                                         ||
                 ' io_work_queue   wkq, '                                      ||
                 ' (SELECT s_delim_str.f_get_string(wkq2.key_data,1, '''       ||
                           cms_dwh_const.C_key_data_delim || ''') AS partner_id, ' ||
                         ' MAX(wkq2.msg_queue_id) AS msg_queue_id '            ||
                  ' FROM   io_work_queue wkq2 '                                ||
                  ' WHERE  wkq2.feed_dtl_id = '|| TO_CHAR(I_feed_id)           ||
                  ' GROUP BY s_delim_str.f_get_string(wkq2.key_data,1, '''     ||
                             cms_dwh_const.C_key_data_delim || ''')) wkq_max ' ||
            ' WHERE  tp.partner_id (+) = '                                     ||
                         ' TO_NUMBER(s_delim_str.f_get_string('                ||
                         ' wkq.key_data,1, '''                                 ||
                           cms_dwh_const.C_key_data_delim  || '''))   '        ||
            ' AND wkq_max.partner_id = s_delim_str.f_get_string ( '            ||
                'wkq.key_data,1, '''||cms_dwh_const.C_key_data_delim|| ''') '  ||
            ' AND wkq_max.msg_queue_id = wkq.msg_queue_id '                    ||
            ' AND wkq.feed_dtl_id  = ' || TO_CHAR(I_feed_id);


        s_info.p_track_process (L_unit,'Select Statement created ',
                                I_log_bool =>FALSE);

        s_util.p_select_dump(
               I_select_string         => L_select_string,
               I_file_name             => L_file_name||s_file_const.C_data_extn,
               I_create_mkr_bool       => TRUE,
               I_no_data_file_if_empty => TRUE,
               I_directory             => s_file_const.C_mkr_dir,
               I_remove_rtn_char       => TRUE);

        s_info.p_track_process (L_unit,
                                L_file_name||' populated',
                                I_log_bool => TRUE);

        s_info.p_track_process (L_unit, s_const.C_end, I_log_bool => TRUE);

    EXCEPTION
    WHEN OTHERS
    THEN

       s_err.p_error_handler;

    END p_extract_trade_partner;

    --
    -- UNIT:        p_extract_trade_ptnr_cntry
    -- DESCRIPTION: This procedures extracts partner country data 
    -- NOTES:
    --
    PROCEDURE p_extract_trade_ptnr_cntry (I_feed_id   IN  s_datatype.feed_id)
    IS

        L_unit            s_datatype.unit_name := 
                                  'dwh_extract_pips.p_extract_trade_ptnr_cntry';
        L_file_name       s_file_sequence.file_name%TYPE := NULL;
        L_select_string   s_datatype.long_query;

    BEGIN

        s_info.p_track_process (L_unit, s_const.C_begin, I_log_bool => TRUE);

        s_util.p_get_file_name(
                       I_file_id   =>  cms_file_const.C_dwh_tdprtnr_cntry_file_id,
                       I_file_type =>  cms_file_const.C_dwh_file_type,
                       O_file_name =>  L_file_name);

        L_select_string := 
            'SELECT s_delim_str.f_get_string(wkq.key_data,1,'''                ||
                    cms_dwh_const.C_key_data_delim  || ''')  || '              ||
            '''' || cms_dwh_const.C_column_delimiter ||   ''''                 ||
            ' || s_delim_str.f_get_string(wkq.key_data,2, '''                  ||
                    cms_dwh_const.C_key_data_delim   || ''') || '              ||
            '''' || cms_dwh_const.C_column_delimiter || ''''                   ||
            ' ||tpc.vat_reg_code || '                                          ||
            '''' || cms_dwh_const.C_column_delimiter || ''''                   ||
            ' || s_delim_str.f_get_string(wkq.key_data,3, '''                  ||
                    cms_dwh_const.C_key_data_delim   || ''')  '                ||
            ' FROM trade_partner_cntry tpc, '                                  ||
                 ' io_work_queue   wkq, '                                      ||
                 ' (SELECT s_delim_str.f_get_string(wkq2.key_data,1, '''       ||
                           cms_dwh_const.C_key_data_delim || ''') AS partner_id, ' ||
                        '  s_delim_str.f_get_string(wkq2.key_data,2, '''       ||
                           cms_dwh_const.C_key_data_delim || ''') AS cntry_lvl_child, '||
                        '  MAX(wkq2.msg_queue_id) AS msg_queue_id '            ||
                  ' FROM   io_work_queue wkq2 '                                ||
                  ' WHERE  wkq2.feed_dtl_id = '|| TO_CHAR(I_feed_id)           ||
                  ' GROUP BY s_delim_str.f_get_string(wkq2.key_data,1, '''     ||
                             cms_dwh_const.C_key_data_delim || '''), '         ||
                  '          s_delim_str.f_get_string(wkq2.key_data,2, '''     ||
                             cms_dwh_const.C_key_data_delim || ''')) wkq_max ' ||
            ' WHERE  tpc.partner_id (+) = '                                    ||
                         ' TO_NUMBER(s_delim_str.f_get_string('                ||
                         ' wkq.key_data,1, '''                                 ||
                           cms_dwh_const.C_key_data_delim  || '''))   '        ||
            ' AND  tpc.cntry_lvl_child (+) = '                                 ||
                         ' TO_NUMBER(s_delim_str.f_get_string('                ||
                         ' wkq.key_data,2, '''                                 ||
                           cms_dwh_const.C_key_data_delim  || '''))   '        ||
            ' AND wkq_max.partner_id = s_delim_str.f_get_string ( '            ||
                'wkq.key_data,1, '''||cms_dwh_const.C_key_data_delim|| ''') '  ||
            ' AND wkq_max.cntry_lvl_child = s_delim_str.f_get_string ( '       ||
                'wkq.key_data,2, '''||cms_dwh_const.C_key_data_delim|| ''') '  ||
            ' AND wkq_max.msg_queue_id = wkq.msg_queue_id '                    ||
            ' AND wkq.feed_dtl_id  = ' || TO_CHAR(I_feed_id);


        s_info.p_track_process (L_unit,'Select Statement created ',
                                I_log_bool =>FALSE);

        s_util.p_select_dump(
               I_select_string         => L_select_string,
               I_file_name             => L_file_name||s_file_const.C_data_extn,
               I_create_mkr_bool       => TRUE,
               I_no_data_file_if_empty => TRUE,
               I_directory             => s_file_const.C_mkr_dir,
               I_remove_rtn_char       => TRUE);

        s_info.p_track_process (L_unit,
                                L_file_name||' populated',
                                I_log_bool => TRUE);

        s_info.p_track_process (L_unit, s_const.C_end, I_log_bool => TRUE);

    EXCEPTION
    WHEN OTHERS
    THEN

       s_err.p_error_handler;

    END p_extract_trade_ptnr_cntry;

    --
    -- UNIT:        p_extract_invoice
    -- DESCRIPTION: This extracts invoice header and detail data
    -- NOTES:

    PROCEDURE p_extract_invoice
    IS

        L_unit s_datatype.unit_name := 'dwh_extract_pips.p_extract_invoice';

        -- strings for sql statements
        L_select_invoice_header    s_datatype.long_query;
        L_select_invoice_line      s_datatype.long_query;

        -- File names for trade_partner and trade_partner_cntry extract
        L_file_name_invoice_header       s_file_sequence.file_name%TYPE := NULL;
        L_file_name_invoice_line         s_file_sequence.file_name%TYPE := NULL;

    BEGIN

        s_info.p_track_process (L_unit, s_const.C_begin, I_log_bool => TRUE);


        -- Get filename for invoice header 
        -- 
        s_util.p_get_file_name(
                       I_file_id   =>  cms_file_const.C_dwh_pips_inv_hdr_file_id,
                       I_file_type =>  cms_file_const.C_dwh_file_type,
                       O_file_name =>  L_file_name_invoice_header);

        -- Get filename for invoice line
        s_util.p_get_file_name(
                       I_file_id   =>  cms_file_const.C_dwh_pips_inv_dtl_file_id,
                       I_file_type =>  cms_file_const.C_dwh_file_type,
                       O_file_name =>  L_file_name_invoice_line);

        L_select_invoice_header :=
              'SELECT invoice_header_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_number||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||fra_invoice_header.partner_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||contact_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_total||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||uplift_total||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_gross_weight||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_net_weight||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_notes_1||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_notes_2||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||gl_extract_dtm||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoiced_date||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||fra_invoice_header.created_dtm||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||fra_invoice_header.last_changed_dtm||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||fra_invoice_header.created_by||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||fra_invoice_header.last_changed_by||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||ship_to_contact_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||tbl_code||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||code||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||tax_total||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||NVL(fra_invoice_header.payment_terms,fra_partner.payment_terms)||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||delivery_dt||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||NVL(fra_invoice_header.partner_invoice_type,fra_partner.partner_invoice_type)'
              ||' FROM fra_invoice_header, ' 
	      ||'      fra_partner '
	      ||' WHERE invoice_number IS NOT NULL '
	      ||' AND   fra_partner.partner_id = fra_invoice_header.partner_id '
	      ||' AND gl_extract_dtm IS NOT NULL'
	      ||' AND  NVL(dwh_extract_ind,'||''''||s_const.C_false||''''||')='||''''||s_const.C_false||'''';

        L_select_invoice_line :=
              'SELECT invoice_line_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_header_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_line_seq||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||prd_lvl_child||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||prd_lvl_master||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||prd_lvl_number||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||prd_mst_number||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||prd_upc||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||manifest_number||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||carton_number||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||to_loc||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||quantity||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||trf_carton_weight||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||trf_carton_net_weight||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||dept_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||option_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||sku_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||colour_code||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||colour_name||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||size_code||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||original_price||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||invoice_price||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||uplift_amount||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||look_descr||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||product_descr||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||retail_descr||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||supplier_id||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||source_name||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||source_code||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||tariff_number||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_percent_1||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_descr_1||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_percent_2||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_descr_2||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_percent_3||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_descr_3||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_percent_4||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_descr_4||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_percent_5||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||composition_descr_5||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||customers_warehouse_attr||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||season_code||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||dept_descr||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||range_descr||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||frn_attr_descr||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||created_dtm||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||last_changed_dtm||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||created_by||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||last_changed_by||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||partner_po_number||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||tax_amount||'
              ||''''||cms_dwh_const.c_column_delimiter||''''
              ||'||comm_amount'
              ||' FROM fra_invoice_line '
		      || ' WHERE invoice_header_id  IN (SELECT invoice_header_id FROM fra_invoice_header'
			  ||' 	WHERE invoice_number IS NOT NULL'
              ||'   AND gl_extract_dtm IS NOT NULL'
              ||'   AND dwh_extract_ind IS  NULL)';


        -- Generate invoice header file 

        s_info.p_track_process (L_unit,'About to write to '||L_file_name_invoice_header,
                                I_log_bool =>TRUE);

        s_util.p_select_dump(
               I_select_string    => L_select_invoice_header,
               I_file_name        => L_file_name_invoice_header|| 
                                                 s_file_const.C_data_extn,
               I_create_mkr_bool  => TRUE,
               I_directory        => s_file_const.C_mkr_dir,
               I_remove_rtn_char  => TRUE);

        s_info.p_track_process (L_unit,L_file_name_invoice_header||' populated',
                                I_log_bool => TRUE);

        s_info.p_track_process (L_unit,'About to write to '||L_file_name_invoice_line,
                                I_log_bool =>TRUE);

        -- Generate invoice line file
        s_util.p_select_dump(
               I_select_string    => L_select_invoice_line,
               I_file_name        => L_file_name_invoice_line|| 
                                                 s_file_const.C_data_extn,
               I_create_mkr_bool  => TRUE,
               I_directory        => s_file_const.C_mkr_dir,
               I_remove_rtn_char  => TRUE);

        s_info.p_track_process (L_unit, L_file_name_invoice_line||' populated',
                                I_log_bool => TRUE);

        -- update records extracted
        UPDATE fra_invoice_header
        SET    dwh_extract_ind = s_const.C_true
        WHERE  NVL(dwh_extract_ind, s_const.C_false) = s_const.C_false
        AND    gl_extract_dtm IS NOT NULL
        AND    invoice_number IS NOT NULL;

        s_info.p_track_process (L_unit, 'dwh_extract_ind updated', I_log_bool => TRUE);

        s_info.p_track_process (L_unit, s_const.C_end, I_log_bool => TRUE);

    EXCEPTION
    WHEN OTHERS
    THEN
        s_err.p_error_handler;

    END p_extract_invoice;

END dwh_extract_pips;
/
SHOW ERRORS
