CREATE OR REPLACE PACKAGE BODY fra_invoice
AS
   --
   -- NAME:  fra_invoice
   -- TYPE:  PL/SQL Package body
   -- TITLE: Package holding plsql supporting creation of franchise and wholesale
   --      : invoices
   -- NOTES:
   --
   --$Revision:   1.46  $
   -------------------------------------------------------------------------------
   -- Version | Date     | Author        | Reason
   -------------------------------------------------------------------------------
   -- 1.0     |23/01/2013| L. Holton     | Original Version
   -- 1.1     |31/01/2013| L. Holton     | Bugfix calculating invoice gross total
   -- 1.2     |31/01/2013| L. Holton     | Add function to return long string
   --         |          |               | of countries of origin for an invoice
   -- 1.3     |22/02/2013| L. Holton     | Add spreadsheet upload procedure
   -- 1.4     |06/03/2013| L. Holton     | Amend join on prd_sku to ean code
   -- 1.5     |15/03/2013| L. Holton     | Modify spreadsheet load
   -- 1.6     |25/03/2013| L. Holton     | add procedure to autoallocate payment
   --         |          |               | to invoices
   -- 1.7     |16/04/2013| L. Holton     | Parameterise autoallocate payment to
   --         |          |               | allow for user selection of invoices
   --         |          |               | to pay.
   -- 1.8     |19/04/2013| L. Holton     | Add procedures to upload non merch
   --         |          |               | spreadsheet and populate invoice
   --         |          |               | tables with this.
   -- 1.9     |13/05/2013| L. Holton     | Modify to cope where there isnt an exact
   --         |          |               | match of lines in fra_invoice_line and
   --         |          |               | loaded in on the spreadsheet
   -- 1.10    |13/05/2013| L. Holton     | Change so the above only called if
   --         |          |               | default fails
   -- 1.11    |16/05/2013| L. Holton     | Modify to allocate credits against
   --         |          |               | invoices
   -- 1.12    |24/05/2013| L. Holton     | Modify spreadsheet upload to allow
   --         |          |               | for quotens in quote enclosed fields
   -- 1.13    |30/05/2013| L.Holton      | Change to invoice in 2 decimal places
   -- 1.14    |03/07/2013| L.Holton      | Bug fix
   -- 1.15    |09/07/2013| L.Holton      | calcuate tax on invoice totals where
   --         |          |               | flag is set on fra_partner
   -- 1.16    |23/07/2013| L.Holton      | Change PO number to varchar type
   -- 1.17    |24/07/2013| L.Holton      | Cater for VAT in payments
   -- 1.18    |08/08/2013| L.Holton      | Improve PO matching
   -- 1.19    |16/08/2013| L.Holton      | Add tolerance to po matching
   -- 1.20    |12/09/2013| L.Holton      | Further improve po matching
   -- 1.21    |19/09/2013| L.Holton      | Add abiltiy to add consignments
   --         |          |               | to an invoice
   -- 1.22    |25/09/2013| L.Holton      | Fix rounding issue on cost price
   -- 1.23    |03/10/2013| L.Holton      | Strip carriage returns from
   --         |          |               | non merch upload lines
   -- 1.24    |04/10/2013| L.Holton      | strip commas from quote delimited numbers
   --         |          |               | and reformat as required from currency to number
   -- 1.25    |07/10/2013| L.Holton      | Add filename parameter to p_create_no_merch_invoice
   -- 1.26    |07/10/2013| L.Holton      | Populate non merch invoice for filename passsed only
   -- 1.27    |08/10/2013| L.Holton      | Tidy up charcter substiutions in non merch upload
   -- 1.28    |09/10/2013| L.Holton      | bug fix and performance fixes to uplift calculation
   -- 1.29    |14/10/2013| L.Holton      | NVL current uplift amount in royalty calculation
   -- 1.30    |17/10/2013| L.Holton      | Bugfix product level uplift
   -- 1.31    |19/12/2013| L.Holton      | re-update fields when calcutaing uplift,
   --         |          |               | will be very useful for Russia
   -- 1.32    |09/07/2014| K.Munir       | Changing lp_update_invoice and p_apply_uplift for
   --         |          |               | Alternate Hierarchies.
   -- 1.33    |16/07/2014| K.Munir       | Removed hardcoding by using package constants
   -- 1.34    |17/07/2014| K.Munir       | Using constants to always use old hierarchy for
   --         |          |               | products
   -- 1.35    |18/09/2014| L.Holton      | Refactor spreadsheet load, better performance
   -- 1.36    |25/09/2014| L.Holton      | Increase size of line buffer
   -- 1.37    |26/09/2014| L.Holton      | Change spreadsheet load loop type
   -- 1.38    |01/10/2014| L.Holton      | remove schema name from create replace
   -- 1.39    |16/10/2014| L.Holton      | Ensure order quantity and delivery week are
   --         |          |               | stripped of spaces and converted to numbers
   --         |          |               | prior to inserting.
   -- 1.40    |20/01/2015| L.Holton      | Post hierarchy performance fix, temporarily remove
   --         |          |               | all uplifts bar the customer one
   -- 1.41    |20/01/2015 | L.Holton     | Bugfix
   -- 1.42    !14/05/2015| N.Blanshard   | Reinstated uplifts removed in 1.40. Fixed
   --         |          |               | hierarchy changes and made performance fix.
   -- 1.43    |01/06/2015| M.Cackett     | Added function f_country_list_smry
   --         |          |               | Used by new Summary Invoice report.
   -- 1.44    |10/06/2015| M. Smart      | PIPS Changes
   --         |          |               | Changed p_create_non_merch_invoice, to supply
   --         |          |               | true indicator value to new invoice header column
   -- 1.45    |16/06/2015| M. Smart      | PIPS Changes
   --         |          |               | Change to lp_update_invoice, to not perform updates to
   --         |          |               | fra_invoice_line if the invoice is a non-merch invoice.
   -- 1.46    |24/10/2017|S. Sharma      | Changes w.r.t. LANDSK-2032
   --         |          |               | Changes to p_create_invoice to insert the transfer number
   --         |          |               | into fra_invoice_line in order to have the correct
   --         |          |               | transfer number in Invoice via PIPS
   -----------------------------------------------------------------------------------------------

   --
   -- UNIT:        p_create_invoice
   -- DESCRIPTION: Create invoice from passed consignment numbers
   -- USAGE:       fra_invoice.p_create_invoice (I_consignment_list
   --      :                                          => L_consignment_list
   --      :                                     I_user
   --      :                                         => L_user)
   --
   -- PARAMS:      I_consignment_list - list of colon delimited consignment
   --       :                            numbers
   --       :      I_user - username of person creating invoice
   -- NOTES:

   TYPE L_comp_rec_type IS RECORD
   (
      percentage    basacdee.atr_code%TYPE,
      description   basahree.atr_header_desc%TYPE
   );

   TYPE L_comp_tbl_type IS TABLE OF L_comp_rec_type
      INDEX BY BINARY_INTEGER;

   L_comp_tbl   L_comp_tbl_type;


    PROCEDURE lp_update_invoice (
        I_invoice_header_id   IN fra_invoice_header.invoice_header_id%TYPE)
    IS
        L_unit                 s_datatype.unit_name := 'fra_invvoice.p_update_invoice';
        L_comp_cur             cms_attr.G_composition_attr_type;
        L_comp_rec             cms_attr.G_composition_record;
        L_comp_tbl             L_comp_tbl_type;
        L_comp_index           PLS_INTEGER;
        L_non_merch_ivc_ind    fra_invoice_header.non_merchandise_ivc_ind%TYPE;
      
    BEGIN
        -- Update ref fields in fra_invoice_line, handy for reflecting changes
        -- in odbms post invoice creation

        <<fetch_non_merch_ind>>
        BEGIN
            SELECT NVL(non_merchandise_ivc_ind,s_const.C_false)
            INTO   L_non_merch_ivc_ind
            FROM   fra_invoice_header
            WHERE  invoice_header_id = I_invoice_header_id;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                L_non_merch_ivc_ind := s_const.C_false;
        END fetch_non_merch_ind;
        
        IF L_non_merch_ivc_ind = s_const.C_false
        THEN
            UPDATE fra_invoice_line fil
            SET    tariff_number =
                       (SELECT cms_attr.f_attr_code_desc 
                                   (cms_attr_const.C_tariff_code_attr,
                                    cms_attr_const.C_tariff_code_type,
                                    cms_attr_const.C_app_fnc_prd,
                                    prdm.prd_lvl_parent)
                        FROM   alt_prdmstee prdm
                        WHERE  prdm.prd_lvl_child = fil.prd_lvl_child
                        AND    prdm.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier),
                   product_descr =
                       (SELECT cms_gen_cus.f_cus_attr_desc (prdm.prd_lvl_parent)
                        FROM   alt_prdmstee prdm
                        WHERE  prdm.prd_lvl_child = fil.prd_lvl_child
                        AND    prdm.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier),
                   look_descr = cms_attr.f_prd_hier_attr_descr 
                                    (cms_attr_const.C_attr_typ_cmp_var,
                                     cms_attr_const.C_end_use_desc,
                                     fil.prd_lvl_child),
                   retail_descr =
                       (SELECT REPLACE(cms_attr.f_cmp_prd_descr (prdm.prd_lvl_parent), '#', ' ')
                        FROM   alt_prdmstee prdm
                        WHERE  prdm.prd_lvl_child = fil.prd_lvl_child
                        AND    prdm.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier)
            WHERE  invoice_header_id = I_invoice_header_id;

            UPDATE fra_invoice_line fil
            SET    range_descr =
                       (SELECT prd3.prd_name_full
                        FROM   alt_prdmstee prd1
                               JOIN alt_prdmstee prd2
                                   ON (    prd2.prd_lvl_child = prd1.prd_lvl_parent
                                       AND prd2.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier)
                               JOIN alt_prdmstee prd3
                                   ON (    prd3.prd_lvl_child = prd2.prd_lvl_parent
                                       AND prd3.prd_lvl_id    = prd_hier.C_range_id
                                       AND prd3.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier)
                        WHERE  prd1.prd_lvl_child = fil.prd_lvl_child
                        AND    prd1.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier),
                   dept_descr =
                       -- 2. Deptartment description
                       (SELECT prd1.prd_name_full
                        FROM   alt_prdmstee prd1
                        WHERE  RTRIM (fil.dept_id) = RTRIM (prd1.prd_lvl_number)
                        AND    prd1.alt_hier_id    = cms_alt_prd_hier.C_prd_legacy_hier
                        AND    prd_lvl_id = prd_hier.C_dept_id),
                   customers_warehouse_attr =
                       --3 customs warehouse attribute where non eu attribute set
                       (SELECT cms_attr.f_attr_code 
                                   (cstm_wh_const.C_customs_non_eu,
                                    cstm_wh_const.C_non_eu_origin,
                                    cms_attr_const.C_app_fnc_prd,
                                    prd1.prd_lvl_parent)
                        FROM   alt_prdmstee prd1
                        WHERE  prd1.prd_lvl_child = fil.prd_lvl_child
                        AND    prd1.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier)
            WHERE  invoice_header_id = I_invoice_header_id;

            -- 3. Custom warehouse attribute where non eu attribute is not set
            UPDATE fra_invoice_line fil
            SET    customers_warehouse_attr =
                       (SELECT cms_attr.f_attr_code (
                               cstm_wh_const.C_customs_warehouse,
                               cstm_wh_const.C_non_eu_origin,
                               cms_attr_const.C_app_fnc_vpc,
                               vpc_tech_key)
                        FROM   vpcprdee vpcp 
                               JOIN alt_prdmstee prd1
                                   ON (vpcp.prd_lvl_child = prd1.prd_lvl_parent)
                        WHERE  prd1.prd_lvl_child = fil.prd_lvl_child
                        AND    prd1.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier)
            WHERE  invoice_header_id = I_invoice_header_id
            AND    customers_warehouse_attr IS NULL;

            -- And lastly populate the composition field
            --
            FOR FV_prd_lvl_parent IN 
                (SELECT prd1.prd_lvl_parent, 
                        prd1.prd_lvl_child
                 FROM   fra_invoice_line fil
                        JOIN alt_prdmstee prd1
                            ON (    prd1.prd_lvl_child = fil.prd_lvl_child
                                AND prd1.alt_hier_id   = cms_alt_prd_hier.C_prd_legacy_hier)
                 WHERE  fil.invoice_header_id = I_invoice_header_id)
            LOOP
                L_comp_tbl.DELETE;

                -- Note that in order to simplify the output of the Composition
                -- Percentage/Description all five occurrences are set

                L_comp_cur := cms_attr.f_attr_composition (FV_prd_lvl_parent.prd_lvl_parent);

                L_comp_index := 0;

                <<composition_loop>>
                LOOP
                    FETCH L_comp_cur INTO L_comp_rec;

                    EXIT WHEN L_comp_cur%NOTFOUND;

                    IF    L_comp_rec.atr_type_desc = cms_attr_const.C_clothing_attr
                       OR L_comp_rec.atr_type_desc = cms_attr_const.C_non_clothing_attr
                    THEN
                        L_comp_index := L_comp_index + 1;
                        L_comp_tbl (L_comp_index).percentage  := L_comp_rec.atr_code;
                        L_comp_tbl (L_comp_index).description := LTRIM (L_comp_rec.atr_header_desc);
                    ELSIF L_comp_rec.atr_code <> cms_attr_const.C_shoe_attr_sole
                    THEN
                        L_comp_index := L_comp_index + 1;
                        L_comp_tbl (L_comp_index).percentage  := '100';
                        L_comp_tbl (L_comp_index).description := LTRIM (L_comp_rec.atr_header_desc);
                    END IF;

                    EXIT WHEN L_comp_index = 5;
                END LOOP composition_loop;

                CLOSE L_comp_cur;

                IF L_comp_index < 5
                THEN
                    FOR I IN L_comp_index + 1 .. 5
                    LOOP
                        L_comp_tbl (I).percentage  := NULL;
                        L_comp_tbl (I).description := NULL;
                    END LOOP;
                END IF;

                UPDATE fra_invoice_line
                SET    composition_percent_1 = L_comp_tbl (1).percentage,
                       composition_descr_1   = L_comp_tbl (1).description,
                       composition_percent_2 = L_comp_tbl (2).percentage,
                       composition_descr_2   = L_comp_tbl (2).description,
                       composition_percent_3 = L_comp_tbl (3).percentage,
                       composition_descr_3   = L_comp_tbl (3).description,
                       composition_percent_4 = L_comp_tbl (4).percentage,
                       composition_descr_4   = L_comp_tbl (4).description,
                       composition_percent_5 = L_comp_tbl (5).percentage,
                       composition_descr_5   = L_comp_tbl (5).description
                WHERE  invoice_header_id = I_invoice_header_id
                AND    prd_lvl_child     = FV_prd_lvl_parent.prd_lvl_child;
            END LOOP;
        END IF; -- non-merch ind is false
    EXCEPTION
        WHEN OTHERS
        THEN
            s_err.p_error_handler;
    END lp_update_invoice;

   PROCEDURE p_create_invoice (
      I_consignment_list     IN     s_datatype.long_string,
      I_user                 IN     s_datatype.user_id,
      IO_invoice_header_id   IN OUT fra_invoice_header.invoice_header_id%TYPE)
   IS
      L_unit                s_datatype.unit_name := 'fra_invoice.p_create_invoice';
      L_invoice_header_id   fra_invoice_header.invoice_header_id%TYPE;
      L_invoice_add_bool    BOOLEAN := FALSE;  -- flag to indicate consignment
   -- being added or create invoice
   BEGIN
      --    s_info.p_track_process (L_unit, s_const.C_begin);



      -- Get the invoice primary key from the sequence

      IF IO_invoice_header_id IS NULL
      THEN
         L_invoice_header_id := fra_invoice_header_seq.NEXTVAL;
         IO_invoice_header_id := L_invoice_header_id;
      ELSE
         L_invoice_header_id := IO_invoice_header_id;
         L_invoice_add_bool := TRUE;
      END IF;

      -- Populate fra_invoice_header if

      IF L_invoice_add_bool = FALSE
      THEN
         INSERT INTO fra_invoice_header (invoice_header_id,
                                         invoice_total,
                                         uplift_total,
                                         invoice_gross_weight,
                                         invoice_net_weight,
                                         created_dtm,
                                         last_changed_dtm,
                                         created_by,
                                         last_changed_by)
            SELECT L_invoice_header_id,
                   SUM (
                        ROUND (cms_util.f_prd_cost (prd_lvl_child), 2)
                      * quantity),
                   0,
                   MAX (
                      (SELECT SUM (trf_carton_wgt)
                         FROM (  SELECT mnfst_number,
                                        carton_number,
                                        trf_carton_wgt
                                   FROM sditrfdte
                                  WHERE INSTR (I_consignment_list,
                                               ':' || mnfst_number || ':') > 0
                               GROUP BY mnfst_number,
                                        carton_number,
                                        trf_carton_wgt))),
                   0,
                   SYSDATE,
                   SYSDATE,
                   I_user,
                   I_user
              FROM sditrfdte
             WHERE INSTR (I_consignment_list, ':' || mnfst_number || ':') > 0;
      END IF;



      -- Populate fra_invoice_line

      INSERT INTO fra_invoice_line (invoice_line_id,
                                    invoice_header_id,
                                    invoice_line_seq,
                                    prd_lvl_child,
                                    prd_lvl_master,
                                    prd_lvl_number,
                                    prd_mst_number,
                                    prd_upc,
                                    manifest_number,
                                    carton_number,
                                    to_loc,
                                    quantity,
                                    trf_carton_weight,
                                    dept_id,
                                    option_id,
                                    sku_id,
                                    colour_code,
                                    colour_name,
                                    size_code,
                                    original_price,
                                    invoice_price,
                                    uplift_amount,
                                    look_descr,
                                    product_descr,
                                    retail_descr,
                                    supplier_id,
                                    source_name,
                                    source_code,
                                    tariff_number,
                                    composition_percent_1,
                                    composition_descr_1,
                                    composition_percent_2,
                                    composition_descr_2,
                                    composition_percent_3,
                                    composition_descr_3,
                                    composition_percent_4,
                                    composition_descr_4,
                                    composition_percent_5,
                                    composition_descr_5,
                                    customers_warehouse_attr,
                                    season_code,
                                    dept_descr,
                                    range_descr,
                                    frn_attr_descr,
                                    created_dtm,
                                    last_changed_dtm,
                                    created_by,
                                    last_changed_by,
                                    trf_number)
         SELECT fra_invoice_line_seq.NEXTVAL,
                L_invoice_header_id,
                1,                                            -- line sequence
                sdi.prd_lvl_child,
                sdi.prd_lvl_master,
                sdi.prd_lvl_number,
                sdi.prd_mst_number,
                pupc.prd_upc,                         -- barcode from prdupcee
                sdi.mnfst_number,                           -- manifest number
                sdi.carton_number,
                sdi.to_loc,                            -- destination location
                sdi.quantity,
                sdi.trf_carton_wgt,                           -- carton weight
                RTRIM (
                   prd_hier.f_tk2higher_id (sdi.prd_lvl_child,
                                            prd_hier.C_dept_id)),  -- dept_id,
                RTRIM (
                   prd_hier.f_tk2higher_id (sdi.prd_lvl_child,
                                            prd_hier.C_option_id)), -- option id
                sdi.prd_lvl_number,                                  -- sku id
                -- prdm, prdmstee to get prd_lvl_parent to pick up option id
                cms_attr.f_attr_code (cms_attr_const.C_attr_typ_cmp_fixed,
                                      cms_attr_const.C_attr_sub_colour,
                                      cms_attr_const.C_app_fnc_prd,
                                      prdm.prd_lvl_parent),    -- colour code,
                cms_attr.f_attr_code_desc (
                   cms_attr_const.C_attr_typ_cmp_fixed,
                   cms_attr_const.C_attr_sub_colour,
                   cms_attr_const.C_app_fnc_prd,
                   prdm.prd_lvl_parent),                        -- colour name
                prdd.prd_pdd_code,                 -- size code, prdd prddmdee
                ROUND (cms_util.f_orig_sell_prc (sdi.prd_lvl_child), 2), -- original price
                ROUND (cms_util.f_prd_cost (sdi.prd_lvl_child), 2), -- invoice cost price,
                0,                      -- uplift amount, calculate seperately
                cms_attr.f_prd_hier_attr_descr (
                   cms_attr_const.C_attr_typ_cmp_var,
                   cms_attr_const.C_end_use_desc,
                   sdi.prd_lvl_child),                      --look description
                cms_gen_cus.f_cus_attr_desc (prdm.prd_lvl_parent), -- product descr
                REPLACE (cms_attr.f_cmp_prd_descr (prdm.prd_lvl_parent),
                         '#',
                         ' '),                           -- retail description
                vpcm.vendor_number,      -- supplier id, vpcm vpcmstee join to
                --  vpcprdee and hence sditrfdte
                basp.cntry_name,      -- source name, name of coutry of origin
                --  bascooee   basp join to vpcprdee   vpcp
                basp.cntry_code,                                -- source code
                cms_attr.f_attr_code_desc (cms_attr_const.C_tariff_code_attr,
                                           cms_attr_const.C_tariff_code_type,
                                           cms_attr_const.C_app_fnc_prd,
                                           prdm.prd_lvl_parent), -- tariff code
                0,                 -- composition percent 1, will update later
                'N/A',         -- composition description 1, will update later
                0,                 -- composition percent 2, will update later
                'N/A',         -- composition description 2, will update later
                0,                 -- composition percent 3, will update later
                'N/A',         -- composition description 3, will update later
                0,                 -- composition percent 4, will update later
                'N/A',         -- composition description 4, will update later
                0,                 -- composition percent 5, will update later
                'N/A',         -- composition description 5, will update later
                ' ',               --customs warehouse attribute, update later
                cms_attr.f_attr_code_desc (
                   cms_attr_const.C_attr_typ_cmp_fixed,
                   cms_attr_const.C_attr_sub_season,
                   cms_attr_const.C_app_fnc_prd,
                   prdm.prd_lvl_parent),                        -- season code
                ' ',                   -- department description ,update later
                ' ',                        -- Range description, update later
                cms_attr.f_prd_hier_attr_descr (
                   cstm_wh_const.C_franchise_trademark,
                   cstm_wh_const.C_trademark_brand,
                   prdm.prd_lvl_parent),                     --trademark/range
                SYSDATE,                                           --  created
                SYSDATE,                                       --  last update
                I_user,                                          -- created by
                I_user,                                     -- last updated by
                sdi.trf_number
           FROM sditrfdte sdi,
                prdmstee prdm,
                vpcprdee vpcp,
                vpcmstee vpcm,
                bascooee basp,
                prdupcee pupc,
                prddmdee prdd,
                prdpdxee prdp
          WHERE     prdm.prd_lvl_child = sdi.prd_lvl_child
                AND vpcm.vpc_tech_key = vpcp.vpc_tech_key
                AND vpcp.prd_lvl_child = prdm.prd_lvl_parent
                AND pupc.vpc_prd_tech_key = vpcp.vpc_prd_tech_key
                AND pupc.prd_lvl_child = prdm.prd_lvl_child
                AND basp.cntry_lvl_child = vpcp.cntry_lvl_child
                AND prdd.prd_pdd_key = prdp.prd_pdd_key
                AND prdp.prd_lvl_child = sdi.prd_lvl_child
                AND INSTR (I_consignment_list, ':' || mnfst_number || ':') >
                       0;


      -- Call local procedure to update invoice fields
      lp_update_invoice (L_invoice_header_id);

      -- Populate fra_invoice_manifest to remove invoiced manifest
      -- numbers from selection screen
      INSERT INTO fra_invoice_manifest (invoice_manifest_id,
                                        invoice_header_id,
                                        manifest_number,
                                        created_dtm,
                                        last_changed_dtm,
                                        created_by,
                                        last_changed_by)
         SELECT fra_invoice_manifest_seq.NEXTVAL,
                L_invoice_header_id,
                manifest_number,
                SYSDATE,
                SYSDATE,
                I_user,
                I_user
           FROM (SELECT DISTINCT manifest_number
                   FROM fra_invoice_line
                  WHERE invoice_header_id = L_invoice_header_id
                 MINUS
                 SELECT manifest_number
                   FROM fra_invoice_manifest
                  WHERE invoice_header_id = L_invoice_header_id);
   --        s_info.p_track_process (L_unit, s_const.C_end);

   --    EXCEPTION

   --    WHEN OTHERS
   --    THEN

   --        s_err.p_error_handler;

   END p_create_invoice;


   --
   -- UNIT:        p_apply_uplift
   -- DESCRIPTION: Applies the uplift to the passed invoice, this can be one or
   --            : more amounts based on cost and/or retail prices.
   -- USAGE      : p_apply_uplift (I_invoice_header_id
   --            :                      => L_invoice_header_id,
   --            :                     I_user
   --            :                     => L_user)
   -- PARAMS     : I_invoice_header_id - unique identifier for a invoice
   --            : I_user - username of person applying the uplift
   -- NOTES:     :
   PROCEDURE p_apply_uplift (
      I_invoice_header_id   IN fra_invoice_header.invoice_header_id%TYPE,
      I_user                IN s_datatype.user_id)
   IS
      L_unit            s_datatype.unit_name := 'fra_invoice.p_apply_uplift';
      L_partner_id      fra_partner.partner_id%TYPE;
      L_vat_ind         s_datatype.ind_var;
      L_tax_auth        txsautee.txs_auth_tech_key%TYPE;
      L_prd_lvl_child   fra_invoice_line.prd_lvl_child%TYPE;
   BEGIN
      -- s_info.p_track_process (L_unit, s_const.C_begin);

      -- Get partner ID
      SELECT partner_id
        INTO L_partner_id
        FROM fra_invoice_header
       WHERE invoice_header_id = I_invoice_header_id;

      -- Four types of uplift, partner, organisation hierarchy,
      -- product hierarchy and product attribute.
      -- These can be exclusive (Standard) or additive (royalty) i.e.
      -- a royatly uplift is added to any existing uplift(s).
      --

      -- Update invoice lines and then update header.
      -- As uplifts can be additive reset when entering this procedur
      UPDATE fra_invoice_line
         SET uplift_amount = NULL
       WHERE invoice_header_id = I_invoice_header_id;

      -- Partner uplift, standard percentage off everything
      UPDATE fra_invoice_line fil
         SET uplift_amount =
                (SELECT (CASE
                            WHEN NVL (fpl.cost_type, 'C') = 'C'
                            THEN
                                 ROUND (
                                      fil1.invoice_price
                                    * (NVL (fpl.ulift_percentage, 0) / 100),
                                    2)
                               + DECODE (NVL (fpl.uplift_type, 'Standard'),
                                         'Standard', 0,
                                         'Royalty', NVL (uplift_amount, 0))
                            ELSE
                                 ROUND (
                                      fil1.original_price
                                    * (NVL (fpl.ulift_percentage, 0) / 100),
                                    2)
                               + DECODE (NVL (fpl.uplift_type, 'Standard'),
                                         'Standard', 0,
                                         'Royalty', NVL (uplift_amount, 0))
                         END)
                   FROM fra_invoice_header fih,
                        fra_prc_uplift fpl,
                        fra_invoice_line fil1
                  WHERE     fih.partner_id = fpl.partner_id
                        AND fil1.invoice_header_id = fih.invoice_header_id
                        AND fih.invoice_header_id = I_invoice_header_id
                        AND fil1.invoice_line_id = fil.invoice_line_id
                        AND fpl.prd_lvl_child IS NULL
                        AND fpl.org_lvl_child IS NULL
                        AND fpl.atr_cod_tech_key IS NULL),
             last_changed_dtm = SYSDATE,
             last_changed_by = I_user
       WHERE     fil.invoice_header_id = I_invoice_header_id
             AND fil.invoice_header_id IN (SELECT invoice_header_id
                                             FROM fra_prc_uplift fpu,
                                                  fra_invoice_header fah1
                                            WHERE     fah1.invoice_header_id =
                                                         I_invoice_header_id
                                                  AND fpu.partner_id =
                                                         fah1.partner_id
                                                  AND fpu.prd_lvl_child
                                                         IS NULL
                                                  AND fpu.org_lvl_child
                                                         IS NULL
                                                  AND fpu.atr_cod_tech_key
                                                         IS NULL);


      -- Country uplift
      UPDATE fra_invoice_line fil
         SET uplift_amount =
                (SELECT (CASE
                            WHEN NVL (fpl.cost_type, 'C') = 'C'
                            THEN
                                 ROUND (
                                      fil2.invoice_price
                                    * (NVL (fpl.ulift_percentage, 0) / 100),
                                    2)
                               + DECODE (NVL (fpl.uplift_type, 'Standard'),
                                         'Standard', 0,
                                         'Royalty', NVL (uplift_amount, 0))
                            ELSE
                                 ROUND (
                                      fil2.original_price
                                    * (NVL (fpl.ulift_percentage, 0) / 100),
                                    2)
                               + DECODE (NVL (fpl.uplift_type, 'Standard'),
                                         'Standard', 0,
                                         'Royalty', NVL (uplift_amount, 0))
                         END)
                   FROM fra_invoice_header fih,
                        fra_prc_uplift fpl,
                        fra_invoice_line fil2,
                        alt_orgmstee o1,
                        alt_orgmstee o2
                  WHERE     o1.alt_hier_id =
                               cms_alt_org_hier.f_default_org_hier
                        AND o2.alt_hier_id =
                               cms_alt_org_hier.f_default_org_hier
                        AND fil.invoice_header_id = fih.invoice_header_id
                        AND fih.invoice_header_id = I_invoice_header_id
                        AND fil2.invoice_line_id = fil.invoice_line_id
                        AND fpl.partner_id = fih.partner_id
                        AND fpl.prd_lvl_child IS NULL
                        AND fpl.atr_cod_tech_key IS NULL
                        AND o2.org_lvl_child = o1.org_lvl_parent
                        AND fpl.org_lvl_child = o2.org_lvl_child
                        AND o1.org_lvl_number = fil2.to_loc),
             last_changed_dtm = SYSDATE,
             last_changed_by = I_user
       WHERE     fil.invoice_header_id = I_invoice_header_id
             AND fil.to_loc IN (SELECT fil2.to_loc
                                  FROM fra_invoice_header fih,
                                       fra_prc_uplift fpl,
                                       fra_invoice_line fil2,
                                       alt_orgmstee o1,
                                       alt_orgmstee o2
                                 WHERE     o1.alt_hier_id =
                                              cms_alt_org_hier.f_default_org_hier
                                       AND o2.alt_hier_id =
                                              cms_alt_org_hier.f_default_org_hier
                                       AND fil.invoice_header_id =
                                              fih.invoice_header_id
                                       AND fih.invoice_header_id =
                                              I_invoice_header_id
                                       AND fpl.partner_id = fih.partner_id
                                       AND fpl.prd_lvl_child IS NULL
                                       AND fpl.atr_cod_tech_key IS NULL
                                       AND fil2.invoice_line_id =
                                              fil.invoice_line_id
                                       AND o2.org_lvl_child =
                                              o1.org_lvl_parent
                                       AND fpl.org_lvl_child =
                                              o2.org_lvl_child
                                       AND o1.org_lvl_number = fil2.to_loc);


      -- Product uplift (slowest - needs some work)
      -- First get prd_lvl_child for uplift (if applicable)
      BEGIN
         SELECT prd_lvl_child
           INTO L_prd_lvl_child
           FROM fra_prc_uplift
          WHERE partner_id = L_partner_id AND prd_lvl_child IS NOT NULL;
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      UPDATE fra_invoice_line fil
         SET uplift_amount =
                (SELECT (CASE
                            WHEN NVL (fpl.cost_type, 'C') = 'C'
                            THEN
                                 ROUND (
                                      fil1.invoice_price
                                    * (NVL (fpl.ulift_percentage, 0) / 100),
                                    2)
                               + DECODE (NVL (fpl.uplift_type, 'Standard'),
                                         'Standard', 0,
                                         'Royalty', NVL (uplift_amount, 0))
                            ELSE
                                 ROUND (
                                      fil1.original_price
                                    * (NVL (fpl.ulift_percentage, 0) / 100),
                                    2)
                               + DECODE (NVL (fpl.uplift_type, 'Standard'),
                                         'Standard', 0,
                                         'Royalty', NVL (uplift_amount, 0))
                         END)
                   FROM fra_invoice_header fih,
                        fra_prc_uplift fpl,
                        fra_invoice_line fil1
                  WHERE     fih.invoice_header_id = I_invoice_header_id
                        AND fil1.invoice_header_id = fih.invoice_header_id
                        AND fpl.partner_id = fih.partner_id
                        AND fpl.prd_lvl_child IS NOT NULL
                        AND fpl.org_lvl_child IS NULL
                        AND fpl.atr_cod_tech_key IS NULL
                        AND fil1.prd_lvl_child IN (    SELECT p.prd_lvl_child
                                                         FROM alt_prdmstee p
                                                        WHERE p.alt_hier_id =
                                                                 cms_alt_prd_hier.C_prd_legacy_hier
                                                   START WITH     p.prd_lvl_child =
                                                                     fpl.prd_lvl_child
                                                              AND p.alt_hier_id =
                                                                     cms_alt_prd_hier.C_prd_legacy_hier
                                                   CONNECT BY     PRIOR p.prd_lvl_child =
                                                                     p.prd_lvl_parent
                                                              AND PRIOR p.alt_hier_id =
                                                                     p.alt_hier_id)
                        AND fil1.invoice_line_id = fil.invoice_line_id),
             last_changed_dtm = SYSDATE,
             last_changed_by = I_user
       WHERE     fil.invoice_header_id = I_invoice_header_id
             AND fil.prd_lvl_child IN (    SELECT p.prd_lvl_child
                                             FROM alt_prdmstee p
                                       START WITH     p.prd_lvl_child IN (SELECT fpu.prd_lvl_child
                                                                            FROM fra_prc_uplift fpu
                                                                           WHERE     fpu.partner_id =
                                                                                        L_partner_id
                                                                                 AND fpu.prd_lvl_child
                                                                                        IS NOT NULL)
                                                  AND p.alt_hier_id =
                                                         cms_alt_prd_hier.C_prd_legacy_hier
                                       CONNECT BY     PRIOR p.prd_lvl_child =
                                                         p.prd_lvl_parent
                                                  AND PRIOR p.alt_hier_id =
                                                         p.alt_hier_id);


      -- Product attribute uplift
      UPDATE fra_invoice_line fil
         SET uplift_amount =
                (SELECT (CASE
                            WHEN NVL (fpl.cost_type, 'C') = 'C'
                            THEN
                                 ROUND (
                                      fil1.invoice_price
                                    * (NVL (fpl.ulift_percentage, 0) / 100),
                                    2)
                               + DECODE (NVL (fpl.uplift_type, 'Standard'),
                                         'Standard', 0,
                                         'Royalty', NVL (uplift_amount, 0))
                            ELSE
                                 ROUND (
                                      fil1.original_price
                                    * (NVL (fpl.ulift_percentage, 0) / 100),
                                    2)
                               + DECODE (NVL (fpl.uplift_type, 'Standard'),
                                         'Standard', 0,
                                         'Royalty', NVL (uplift_amount, 0))
                         END)
                   FROM fra_invoice_header fih,
                        fra_prc_uplift fpl,
                        fra_invoice_line fil1,
                        basatpee b
                  WHERE     fil.invoice_header_id = fih.invoice_header_id
                        AND fpl.partner_id = fih.partner_id
                        AND fpl.prd_lvl_child IS NULL
                        AND fpl.org_lvl_child IS NULL
                        AND b.prd_lvl_child = fil1.prd_lvl_child
                        AND fpl.atr_cod_tech_key = b.atr_cod_tech_key
                        AND fil1.invoice_line_id = fil.invoice_line_id),
             last_changed_dtm = SYSDATE,
             last_changed_by = I_user
       WHERE     fil.invoice_header_id = I_invoice_header_id
             AND fil.prd_lvl_child IN (SELECT fil1.prd_lvl_child
                                         FROM fra_invoice_header fih,
                                              fra_prc_uplift fpl,
                                              fra_invoice_line fil1,
                                              basatpee b
                                        WHERE     fil.invoice_header_id =
                                                     fih.invoice_header_id
                                              AND fpl.partner_id =
                                                     fih.partner_id
                                              AND fpl.prd_lvl_child IS NULL
                                              AND fpl.org_lvl_child IS NULL
                                              AND b.prd_lvl_child =
                                                     fil.prd_lvl_child
                                              AND fpl.atr_cod_tech_key =
                                                     b.atr_cod_tech_key);


      -- Update header, at this point we can determine if its a cost or
      -- retail partner so calculate invoice net total accordingly
      UPDATE fra_invoice_header fih
         SET uplift_total =
                (SELECT SUM (NVL (fil.uplift_amount, 0) * quantity)
                   FROM fra_invoice_line fil
                  WHERE fil.invoice_header_id = fih.invoice_header_id),
             invoice_total =
                (  SELECT (CASE
                              WHEN NVL (p.partner_invoice_type, 'C') = 'C'
                              THEN
                                 SUM (NVL (fil.invoice_price, 0) * quantity)
                              ELSE
                                 SUM (NVL (fil.original_price, 0) * quantity)
                           END)
                     FROM fra_partner p, fra_invoice_line fil
                    WHERE     p.partner_id = fih.partner_id
                          AND fil.invoice_header_id = fih.invoice_header_id
                 GROUP BY NVL (p.partner_invoice_type, 'C')),
             last_changed_dtm = SYSDATE,
             last_changed_by = I_user
       WHERE fih.invoice_header_id = I_invoice_header_id;


      -- update fields in invoice
      lp_update_invoice (I_invoice_header_id);

      -- Update w_fra_partner_prd_po to set invoice_header_id for invoiced upcs
      --
      UPDATE w_fra_partner_prd_po w
         SET invoice_header_id = I_invoice_header_id
       WHERE     partner_id = L_partner_id
             AND ean_code IN (  SELECT TO_CHAR (prd_upc)
                                  FROM fra_invoice_line
                                 WHERE     invoice_header_id =
                                              I_invoice_header_id
                                       AND TO_CHAR (prd_upc) = w.ean_code
                              --HAVING w.actual_allocated = SUM(NVL(quantity,0))
                              GROUP BY TO_CHAR (prd_upc))
             AND invoice_header_id IS NULL
             AND partner_po_number IS NOT NULL
             AND (partner_id,
                  ean_code,
                  actual_allocated,
                  created_dtm) IN (  SELECT partner_id,
                                            ean_code,
                                            actual_allocated,
                                            MIN (created_dtm)
                                       FROM w_fra_partner_prd_po
                                      WHERE invoice_header_id IS NULL
                                   GROUP BY partner_id,
                                            ean_code,
                                            actual_allocated);

      -- Update fra_invoice_line to set partner_po_number for invoiced upcs
      BEGIN
         -- First do default action
         UPDATE fra_invoice_line fil
            SET partner_po_number =
                   (  SELECT partner_po_number
                        FROM w_fra_partner_prd_po w
                       WHERE     invoice_header_id = I_invoice_header_id
                             AND ean_code = fil.prd_upc
                             AND partner_po_number IS NOT NULL
                    GROUP BY partner_po_number)
          WHERE     invoice_header_id = I_invoice_header_id
                AND partner_po_number IS NULL
                AND prd_upc IN (SELECT ean_code
                                  FROM w_fra_partner_prd_po w
                                 WHERE invoice_header_id =
                                          I_invoice_header_id);
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      BEGIN
         -- Update exact matches

         UPDATE fra_invoice_line fil
            SET partner_po_number =
                   (SELECT MAX (w.partner_po_number)
                      FROM w_fra_partner_prd_po w, fra_invoice_line fil2
                     WHERE     w.invoice_header_id = fil.invoice_header_id
                           AND w.ean_code = TO_CHAR (fil.prd_upc)
                           AND TO_CHAR (fil2.prd_upc) = w.ean_code
                           AND fil2.prd_upc = fil.prd_upc
                           AND w.actual_allocated = fil2.quantity
                           AND w.partner_po_number IS NOT NULL)
          WHERE     invoice_header_id = I_invoice_header_id
                AND partner_po_number IS NULL
                AND EXISTS
                       (SELECT 'x'
                          FROM w_fra_partner_prd_po w1
                         WHERE     w1.invoice_header_id =
                                      fil.invoice_header_id
                               AND w1.ean_code = TO_CHAR (fil.prd_upc));
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      -- Next update aggregates
      BEGIN
         UPDATE fra_invoice_line fil
            SET partner_po_number =
                   (  SELECT w.partner_po_number
                        FROM w_fra_partner_prd_po w, fra_invoice_line fil2
                       WHERE     w.invoice_header_id = fil.invoice_header_id
                             AND fil2.invoice_header_id = w.invoice_header_id
                             AND fil2.prd_upc = fil.prd_upc
                             AND w.partner_po_number IS NOT NULL
                             AND w.ean_code = TO_CHAR (fil2.prd_upc)
                             AND w.product_code = RTRIM (fil2.option_id)
                             AND TO_CHAR (fil2.prd_upc) = w.ean_code
                             AND w.partner_po_number =
                                    (SELECT MAX (w2.partner_po_number)
                                       FROM w_fra_partner_prd_po w2
                                      WHERE     w2.invoice_header_id =
                                                   w.invoice_header_id
                                            AND w2.ean_code = w.ean_code
                                            AND w2.actual_allocated =
                                                   w.actual_allocated)
                      HAVING w.actual_allocated = SUM (NVL (fil2.quantity, 0))
                    GROUP BY w.partner_po_number,
                             w.ean_code,
                             TO_CHAR (fil2.prd_upc),
                             w.actual_allocated,
                             w.product_code,
                             w.invoice_header_id)
          WHERE     invoice_header_id = I_invoice_header_id
                AND partner_po_number IS NULL
                AND EXISTS
                       (SELECT 'x'
                          FROM w_fra_partner_prd_po w1
                         WHERE     w1.invoice_header_id =
                                      fil.invoice_header_id
                               AND w1.ean_code = TO_CHAR (fil.prd_upc));
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      BEGIN
         -- Update mismatches

         UPDATE fra_invoice_line fil
            SET partner_po_number =
                   (  SELECT DISTINCT w.partner_po_number
                        FROM fra_invoice_line fil2,
                             w_fra_partner_prd_po w,
                             fra_invoice_header fih
                       WHERE     fil2.invoice_header_id = fil.invoice_header_id
                             AND fih.invoice_header_id = fil.invoice_header_id
                             AND fil2.partner_po_number IS NULL
                             AND fil2.option_id = fil.option_id
                             AND fil2.prd_upc = fil.prd_upc
                             AND w.product_code = RTRIM (fil.option_id)
                             AND w.ean_code = TO_CHAR (fil.prd_upc)
                             AND w.partner_id = fih.partner_id
                             AND w.actual_allocated > 0
                             AND w.created_dtm >= TRUNC (SYSDATE - 15)
                             AND fil.to_loc != 980
                             AND fil.partner_po_number IS NULL
                      HAVING SUM (fil2.quantity) != actual_allocated
                    GROUP BY fil2.invoice_header_id,
                             fil2.option_id,
                             fil2.prd_upc,
                             w.actual_allocated,
                             w.partner_po_number,
                             w.file_name,
                             w.ean_code,
                             w.invoice_header_id)
          WHERE     (invoice_header_id, option_id, prd_upc) IN (  SELECT DISTINCT
                                                                         fil.invoice_header_id,
                                                                         fil.option_id,
                                                                         fil.prd_upc
                                                                    FROM fra_invoice_line fil,
                                                                         fra_invoice_header fih,
                                                                         w_fra_partner_prd_po w
                                                                   WHERE     fil.invoice_header_id =
                                                                                I_invoice_header_id
                                                                         AND fih.invoice_header_id =
                                                                                fil.invoice_header_id
                                                                         AND w.product_code =
                                                                                RTRIM (
                                                                                   fil.option_id)
                                                                         AND w.ean_code =
                                                                                TO_CHAR (
                                                                                   fil.prd_upc)
                                                                         AND w.partner_id =
                                                                                fih.partner_id
                                                                         AND w.created_dtm >=
                                                                                TRUNC (
                                                                                     SYSDATE
                                                                                   - 15)
                                                                         AND fil.to_loc !=
                                                                                980
                                                                         AND fil.partner_po_number
                                                                                IS NULL
                                                                  HAVING SUM (
                                                                            quantity) !=
                                                                            actual_allocated
                                                                GROUP BY fil.invoice_header_id,
                                                                         option_id,
                                                                         prd_upc,
                                                                         w.actual_allocated,
                                                                         w.partner_po_number,
                                                                         w.file_name,
                                                                         w.ean_code,
                                                                         w.invoice_header_id)
                AND fil.invoice_header_id = I_invoice_header_id
                AND fil.partner_po_number IS NULL;
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      -- Any others
      BEGIN
         UPDATE fra_invoice_line fil
            SET partner_po_number =
                   (  SELECT DISTINCT w.partner_po_number
                        FROM fra_invoice_line fil2, w_fra_partner_prd_po w
                       WHERE     fil2.invoice_header_id = fil.invoice_header_id
                             AND fil2.partner_po_number IS NULL
                             AND fil2.option_id = fil.option_id
                             AND fil2.prd_upc = fil.prd_upc
                             AND w.product_code = RTRIM (fil.option_id)
                             AND w.ean_code = TO_CHAR (fil.prd_upc)
                             AND w.partner_id = L_partner_id
                             AND w.actual_allocated > 0
                             AND w.created_dtm >= TRUNC (SYSDATE - 15)
                             AND fil.partner_po_number IS NULL
                      HAVING SUM (fil2.quantity) = actual_allocated
                    GROUP BY fil2.invoice_header_id,
                             fil2.option_id,
                             fil2.prd_upc,
                             w.actual_allocated,
                             w.partner_po_number,
                             w.file_name,
                             w.ean_code,
                             w.invoice_header_id)
          WHERE     (invoice_header_id, option_id, prd_upc) IN (  SELECT DISTINCT
                                                                         fil.invoice_header_id,
                                                                         fil.option_id,
                                                                         fil.prd_upc
                                                                    FROM fra_invoice_line fil,
                                                                         w_fra_partner_prd_po w
                                                                   WHERE     fil.invoice_header_id =
                                                                                I_invoice_header_id
                                                                         AND w.product_code =
                                                                                RTRIM (
                                                                                   fil.option_id)
                                                                         AND w.ean_code =
                                                                                TO_CHAR (
                                                                                   fil.prd_upc)
                                                                         AND w.partner_id =
                                                                                L_partner_id
                                                                         AND w.created_dtm >=
                                                                                TRUNC (
                                                                                     SYSDATE
                                                                                   - 15)
                                                                         AND fil.partner_po_number
                                                                                IS NULL
                                                                  HAVING SUM (
                                                                            quantity) =
                                                                            actual_allocated
                                                                GROUP BY fil.invoice_header_id,
                                                                         option_id,
                                                                         prd_upc,
                                                                         w.actual_allocated,
                                                                         w.partner_po_number,
                                                                         w.file_name,
                                                                         w.ean_code,
                                                                         w.invoice_header_id)
                AND fil.partner_po_number IS NULL;
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      -- calculate tax/vat where applicable
      SELECT NVL (vat_ind, 'F')
        INTO L_vat_ind
        FROM fra_partner fp, fra_invoice_header fih
       WHERE     fih.invoice_header_id = I_invoice_header_id
             AND fp.partner_id = fih.partner_id;

      IF L_vat_ind = 'T'
      THEN
         --
         -- Get tax auth tech key for partner
         SELECT cms_tax.f_iso_tax_auth_tech_key (NULL, partner_country_code)
           INTO L_tax_auth
           FROM fra_partner
          WHERE partner_id = L_partner_id;

         -- calulate tax, first at line level and then roll up to invoice level

         UPDATE fra_invoice_line fil
            SET tax_amount =
                   (SELECT   (  (CASE
                                    WHEN fp.partner_invoice_type = 'C'
                                    THEN
                                         fil2.invoice_price
                                       + fil2.uplift_amount
                                    ELSE
                                         fil2.original_price
                                       + fil2.uplift_amount
                                 END)
                              * quantity)
                           * (  cms_tax.f_iso_tax_rate (L_tax_auth,
                                                        fil.created_dtm,
                                                        fil.prd_lvl_child)
                              / 100)
                      FROM fra_invoice_line fil2,
                           fra_invoice_header fih,
                           fra_partner fp
                     WHERE     fil2.invoice_line_id = fil.invoice_line_id
                           AND fih.invoice_header_id = fil2.invoice_header_id
                           AND fp.partner_id = fih.partner_id)
          WHERE fil.invoice_header_id = I_invoice_header_id;

         -- Now invoice level

         UPDATE fra_invoice_header fih
            SET tax_total =
                   (SELECT SUM (NVL (tax_amount, 0))
                      FROM fra_invoice_line fil
                     WHERE fil.invoice_header_id = fih.invoice_header_id)
          WHERE invoice_header_id = I_invoice_header_id;
      END IF;
   --    EXCEPTION
   --    WHEN OTHERS
   --    THEN

   --        s_err.p_error_handler;



   END p_apply_uplift;


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
   FUNCTION f_country_list (
      I_invoice_header_id   IN fra_invoice_header.invoice_header_id%TYPE)
      RETURN VARCHAR2
   IS
      L_unit           s_datatype.unit_name := 'fra_invoice.f_country_list';
      L_country_list   s_datatype.long_string;
   BEGIN
      FOR FV_source_name IN (SELECT DISTINCT source_name
                               FROM fra_invoice_line
                              WHERE invoice_header_id = I_invoice_header_id)
      LOOP
         L_country_list := L_country_list || ' ' || FV_source_name.source_name;
      END LOOP;

      L_country_list := LTRIM (L_country_list);

      RETURN (L_country_list);
   EXCEPTION
      WHEN OTHERS
      THEN
         s_err.p_error_handler;
   END f_country_list;

    --
    -- UNIT:        f_country_list_smry
    -- DESCRIPTION: Returns a string containing all the 
    --            : countries of the suppliers in a summary invoice
    -- NOTES:
    --
    FUNCTION f_country_list_smry (I_summary_invoice_id IN 
                                 pips_multi_invoice_smry.summary_invoice_id%TYPE)
    RETURN VARCHAR2
    IS
      L_unit           s_datatype.unit_name := 'fra_invoice.f_country_list_smry';
      L_country_list   s_datatype.long_string;
    BEGIN
      FOR FV_source_name IN (
          SELECT DISTINCT fil.source_name
          FROM pips_multi_invoice_smry pmis
          JOIN fra_invoice_header fih
               ON (pmis.invoice_number = fih.invoice_number
               AND pmis.partner_id = fih.partner_id)
          JOIN fra_invoice_line fil
               ON (fih.invoice_header_id = fil.invoice_header_id)
          WHERE pmis.summary_invoice_id = I_summary_invoice_id
          )
      LOOP
         L_country_list := L_country_list || ' ' || FV_source_name.source_name;
      END LOOP;

      L_country_list := LTRIM (L_country_list);
    
      RETURN (L_country_list);
   EXCEPTION
      WHEN OTHERS
      THEN
         s_err.p_error_handler;
   END f_country_list_smry;

   
   
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
   PROCEDURE p_upload_spreadsheet (
      I_partner_id   IN fra_partner.partner_id%TYPE,
      I_blob_name    IN VARCHAR2,
      I_user         IN s_datatype.user_id)
   IS
      L_blob_length        NUMBER := 0;
      L_chunk_size         NUMBER := 75;
      L_position           NUMBER := 1;
      L_line               VARCHAR2 (20000);
      L_raw_buffer         RAW (100);
      L_char_buffer        VARCHAR2 (8000);
      L_Line_count         NUMBER := 1;
      L_document           BLOB;
      L_unit               s_datatype.unit_name
                              := 'fra_invoice.p_upload_spreadsheet';
      L_open_quote_pos     NUMBER := 0;
      L_close_quote_pos    NUMBER := 0;
      L_apex_security_id   NUMBER;
      L_file_name          VARCHAR2 (100)
         := SUBSTR (I_blob_name, INSTR (I_blob_name, '/') + 1);
      L_eol                VARCHAR2 (2) := CHR (13) || CHR (10);
      L_nbsp               VARCHAR2 (1) := CHR (160);
      L_sp                 VARCHAR2 (1) := CHR (32);
   BEGIN
      -- set up security
      SELECT MAX (workspace_id) INTO L_apex_security_id FROM apex_workspaces;

      wwv_flow_api.set_security_group_id (L_apex_security_id);

      -- clear out previous spreadsheet entries for partner
      DELETE FROM w_fra_partner_prd_po
            WHERE partner_id = I_partner_id AND file_name = L_file_name;

      -- Get blob to process
      SELECT blob_content
        INTO L_document
        FROM wwv_flow_files
       WHERE name = I_blob_name;

      L_blob_length := DBMS_LOB.getlength (L_document);


      -- Read and convert raw to varchar
      --WHILE (L_position <= L_blob_length)
      LOOP
         L_raw_buffer :=
            DBMS_LOB.SUBSTR (L_document, L_chunk_size, L_position);
         L_char_buffer :=
            L_char_buffer || UTL_RAW.cast_to_varchar2 (L_raw_buffer);

         -- L_position  := L_position + L_chunk_size;

         -- When a whole line is retrieved
         IF INSTR (L_char_buffer, L_eol) > 0
         THEN
            L_line :=
                  L_line
               || SUBSTR (L_char_buffer, 1, INSTR (L_char_buffer, L_eol) - 1);

            -- process line, first line is for headers so ignore
            IF L_line_count > 1
            THEN
               DBMS_OUTPUT.put_line (L_line);
               -- First need to ensure quotens in section surrounded by quotes are transformed to
               -- ~, stops the string being decomposed incorrectly. There are two fields that can have
               -- this.
               --
               L_open_quote_pos :=
                  INSTR (L_line,
                         '"',
                         1,
                         1);
               L_close_quote_pos :=
                  INSTR (L_line,
                         '"',
                         1,
                         2);

               IF     L_open_quote_pos > 0
                  AND L_close_quote_pos > L_open_quote_pos
               THEN
                  L_line :=
                        SUBSTR (L_line, 1, L_open_quote_pos - 1)
                     || REPLACE (
                           SUBSTR (L_LINE,
                                   L_open_quote_pos,
                                   L_close_quote_pos - L_open_quote_pos + 1),
                           ',',
                           '~')
                     || SUBSTR (L_line, L_close_quote_pos + 1);
               END IF;

               --  process 2nd field

               L_open_quote_pos :=
                  INSTR (L_line,
                         '"',
                         1,
                         3);
               L_close_quote_pos :=
                  INSTR (L_line,
                         '"',
                         1,
                         4);

               IF (    L_open_quote_pos > 0
                   AND L_close_quote_pos > L_open_quote_pos)
               THEN
                  L_line :=
                        SUBSTR (L_line, 1, L_open_quote_pos - 1)
                     || REPLACE (
                           SUBSTR (L_LINE,
                                   L_open_quote_pos,
                                   L_close_quote_pos - L_open_quote_pos + 1),
                           ',',
                           '~')
                     || SUBSTR (L_line, L_close_quote_pos + 1);
               END IF;

               -- Replace any non breaking space, chr(160) ,with space chr(32)
               L_line := REPLACE (L_line, L_nbsp, L_sp);

               -- now decompose delimited line to compoments
               -- and insert into table
               BEGIN
                  INSERT INTO w_fra_partner_prd_po (file_name,
                                                    partner_id,
                                                    partner_po_number,
                                                    parent_sku,
                                                    child_sku,
                                                    mda,
                                                    dept_descr,
                                                    product_descr,
                                                    product_code,
                                                    ean_code,
                                                    colour_name,
                                                    retail_price,
                                                    size_code,
                                                    order_quantity,
                                                    delivery_week,
                                                    actual_allocated,
                                                    allocated_dt,
                                                    comments,
                                                    created_dtm,
                                                    last_changed_dtm,
                                                    created_by,
                                                    last_changed_by)
                          VALUES (
                                    L_file_name,
                                    I_partner_id,
                                    s_delim_str.f_get_string (L_line, 1, ','), -- partner po number
                                    s_delim_str.f_get_string (L_line, 2, ','), -- parent sku
                                    s_delim_str.f_get_string (L_line, 3, ','), -- child sku
                                    s_delim_str.f_get_string (L_line, 4, ','), -- mda
                                    REPLACE (
                                       REPLACE (
                                          s_delim_str.f_get_string (L_line,
                                                                    5,
                                                                    ','),
                                          '~',
                                          ','),
                                       '"',
                                       NULL),        -- department description
                                    REPLACE (
                                       REPLACE (
                                          s_delim_str.f_get_string (L_line,
                                                                    6,
                                                                    ','),
                                          '~',
                                          ','),
                                       '"',
                                       NULL),           -- product description
                                    TRIM (
                                       s_delim_str.f_get_string (L_line,
                                                                 7,
                                                                 ',')), -- product code
                                    s_delim_str.f_get_string (L_line, 8, ','), -- ean code
                                    s_delim_str.f_get_string (L_line, 9, ','), --  colour name
                                    s_delim_str.f_get_string (L_line,
                                                              10,
                                                              ','), -- retail price
                                    s_delim_str.f_get_string (L_line,
                                                              11,
                                                              ','), -- size code
                                    TO_NUMBER (
                                       REPLACE (
                                          s_delim_str.f_get_string (L_line,
                                                                    12,
                                                                    ','),
                                          L_sp,
                                          NULL)),            -- order quantity
                                    TO_NUMBER (
                                       REPLACE (
                                          s_delim_str.f_get_string (L_line,
                                                                    13,
                                                                    ','),
                                          L_sp,
                                          NULL)),             -- delivery week
                                    s_delim_str.f_get_string (L_line,
                                                              14,
                                                              ','), -- actual number allocated
                                    TO_DATE (
                                       s_delim_str.f_get_string (L_line,
                                                                 15,
                                                                 ','),
                                       'DD/MM/RRRR'),          -- allocated dt
                                    s_delim_str.f_get_string (L_line,
                                                              16,
                                                              ','), -- comments
                                    SYSDATE,                    -- created dtm
                                    SYSDATE,               -- last changed dtm
                                    I_user,                      -- created by
                                    I_user                  -- last updated by
                                          );
               EXCEPTION
                  WHEN OTHERS
                  THEN
                     DBMS_OUTPUT.put_line ('ERROR ' || L_line);
               --    RAISE;

               END;
            END IF;

            -- Clear out

            L_line := NULL;
            L_char_buffer :=
               SUBSTR (L_char_buffer, INSTR (L_char_buffer, L_eol) + 2);
            L_line_count := L_line_count + 1;
         END IF;

         EXIT WHEN (L_position > L_blob_length) AND L_char_buffer IS NULL;
         L_position := L_position + L_chunk_size;
      END LOOP;
   END p_upload_spreadsheet;

   --
   -- UNIT       : p_apply_payment
   -- DESCRIPTION: Takes the payment and applies to invoices starting from
   --            : from the oldest that isnt fully paid
   -- USAGE      : fra_invoice.p_apply_payment (I_payment_id => L_payment_id,
   --            :                              I_user => L_user,
   --            :                              I_invoice_list => L_invoice_list);
   -- PARAMS     : I_payment_id  - ID of payment made
   --            : I_user        - username of person applying the payment
   --            : I_invoice_list - list of invoices
   -- NOTES      :
   PROCEDURE p_apply_payment (
      I_payment_id     IN fra_payment.payment_id%TYPE,
      I_user           IN s_datatype.user_id,
      I_invoice_list   IN s_datatype.long_string DEFAULT NULL)
   IS
      L_payment_amount    NUMBER (10, 2) := 0;
      L_balance           NUMBER (10, 2) := 0;
      L_payment_partner   NUMBER (10);
      L_invoice_total     NUMBER (10, 2) := 0;
   BEGIN
        -- get payment amount balance, i.e. after any invoice payments have
        -- been made against it
        SELECT fpay.payment_amount - NVL (SUM (fip.payment_amount), 0),
               partner_id
          INTO L_payment_amount, L_payment_partner
          FROM fra_payment fpay, fra_invoice_payment fip
         WHERE     fpay.payment_id = I_payment_id
               AND fip.payment_id(+) = fpay.payment_id
      GROUP BY fpay.payment_amount, partner_id;


     <<open_invoice>>
      FOR FV_invoice
         IN (  SELECT fih2.partner_id,
                      fih2.invoice_number,
                      fih2.invoice_header_id,
                        fih2.invoice_total
                      + fih2.uplift_total
                      + NVL (fih2.tax_total, 0)
                         AS invoice_total,
                      NVL (
                         (SELECT SUM (fip.payment_amount)
                            FROM fra_invoice_payment fip
                           WHERE fip.invoice_header_id = fih2.invoice_header_id),
                         0)
                         AS invoice_payment_total
                 FROM fra_invoice_header fih2
                WHERE     NVL (
                             (SELECT SUM (fip.payment_amount)
                                FROM fra_invoice_payment fip
                               WHERE fip.invoice_header_id =
                                        fih2.invoice_header_id),
                             0) <
                             (  fih2.invoice_total
                              + fih2.uplift_total
                              + NVL (fih2.tax_total, 0))
                      AND partner_id = L_payment_partner
                      AND INSTR (
                             NVL (I_invoice_list,
                                  ':' || invoice_header_id || ':'),
                             ':' || invoice_header_id || ':') > 0
             ORDER BY NVL (invoiced_date, created_dtm))
      LOOP
         IF L_payment_amount > 0
         THEN
            -- Some balance left, insert fra_invoice_payment record
            -- Determine if part or full payment
            L_invoice_total :=
               FV_invoice.invoice_total - FV_invoice.invoice_payment_total;
            L_balance := L_payment_amount - L_invoice_total;

            IF L_balance < 0
            THEN
               L_invoice_total := L_payment_amount;
            END IF;

            L_payment_amount := L_payment_amount - L_invoice_total;

            INSERT INTO fra_invoice_payment (invoice_payment_id,
                                             payment_id,
                                             invoice_header_id,
                                             payment_amount,
                                             created_dtm,
                                             last_changed_dtm,
                                             created_by,
                                             last_changed_by)
                 VALUES (fra_invoice_payment_seq.NEXTVAL,
                         I_payment_id,
                         FV_invoice.invoice_header_id,
                         L_invoice_total,
                         SYSDATE,
                         SYSDATE,
                         I_user,
                         I_user);
         END IF;
      END LOOP open_invoice;
   EXCEPTION
      WHEN OTHERS
      THEN
         s_err.p_error_handler;
   END p_apply_payment;

   --
   -- UNIT       : p_apply_trans
   -- DESCRIPTION: Takes the transaction (credit/debit) and applies to
   --            : list of invoices supplied
   -- USAGE      : fra_invoice.p_apply_trans (I_transaction_id => L_transaction_id,
   --            :                              I_user => L_user,
   --            :                              I_invoice_list => L_invoice_list);
   -- PARAMS     : I_transaction_id  - ID of transaction made (credit)
   --            : I_user        - username of person applying the payment
   --            : I_invoice_list - List of invoices to apply transaction to
   -- NOTES      :
   PROCEDURE p_apply_trans (
      I_transaction_id   IN fra_partner_trans.transaction_id%TYPE,
      I_user             IN s_datatype.user_id,
      I_invoice_list     IN s_datatype.long_string DEFAULT NULL)
   IS
      L_trans_amount    NUMBER (10, 2) := 0;
      L_balance         NUMBER (10, 2) := 0;
      L_trans_partner   NUMBER (10);
      L_invoice_total   NUMBER (10, 2) := 0;
      L_trans_type      fra_partner_trans.transaction_type%TYPE;
      L_trans_sign      NUMBER (1) := 1;
   BEGIN
        -- get payment amount balance, i.e. after any invoice payments have
        -- been made against it
        SELECT   fpay.transaction_amount
               - NVL (SUM (ABS (fip.payment_amount)), 0),
               partner_id,
               transaction_type
          INTO L_trans_amount, L_trans_partner, L_trans_type
          FROM fra_partner_trans fpay, fra_invoice_payment fip
         WHERE     fpay.transaction_id = I_transaction_id
               AND fip.transaction_id(+) = fpay.transaction_id
      GROUP BY fpay.transaction_amount, partner_id, transaction_type;

      IF L_trans_type = 'D'
      THEN
         L_trans_sign := -1;
      END IF;


     <<open_invoice>>
      FOR FV_invoice
         IN (  SELECT fih2.partner_id,
                      fih2.invoice_number,
                      fih2.invoice_header_id,
                        fih2.invoice_total
                      + fih2.uplift_total
                      + NVL (fih2.tax_total, 0)
                         AS invoice_total,
                      NVL (
                         (SELECT SUM (fip.payment_amount)
                            FROM fra_invoice_payment fip
                           WHERE fip.invoice_header_id = fih2.invoice_header_id),
                         0)
                         AS invoice_payment_total
                 FROM fra_invoice_header fih2
                WHERE     partner_id = L_trans_partner
                      AND INSTR (
                             NVL (I_invoice_list,
                                  ':' || invoice_header_id || ':'),
                             ':' || invoice_header_id || ':') > 0
             ORDER BY NVL (invoiced_date, created_dtm))
      LOOP
         IF L_trans_amount > 0
         THEN
            -- Some balance left, insert fra_invoice_payment record
            -- Determine if part or full payment
            L_invoice_total :=
                 FV_invoice.invoice_total
               - (L_trans_sign * FV_invoice.invoice_payment_total);
            L_balance := L_trans_amount - L_invoice_total;

            IF L_balance < 0
            THEN
               L_invoice_total := L_trans_amount;
            END IF;

            L_trans_amount := L_trans_amount - L_invoice_total;

            INSERT INTO fra_invoice_payment (invoice_payment_id,
                                             transaction_id,
                                             invoice_header_id,
                                             payment_amount,
                                             created_dtm,
                                             last_changed_dtm,
                                             created_by,
                                             last_changed_by)
                 VALUES (fra_invoice_payment_seq.NEXTVAL,
                         I_transaction_id,
                         FV_invoice.invoice_header_id,
                         L_invoice_total * L_trans_sign,
                         SYSDATE,
                         SYSDATE,
                         I_user,
                         I_user);
         END IF;
      END LOOP open_invoice;
   EXCEPTION
      WHEN OTHERS
      THEN
         s_err.p_error_handler;
   END p_apply_trans;

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
   PROCEDURE p_upload_non_merch (
      I_partner_id   IN fra_partner.partner_id%TYPE,
      I_blob_name    IN VARCHAR2,
      I_user         IN s_datatype.user_id)
   IS
      L_blob_length        NUMBER := 0;
      L_chunk_size         NUMBER := 4;
      L_position           NUMBER := 1;
      L_line               VARCHAR2 (20000);
      L_raw_buffer         RAW (100);
      L_char_buffer        VARCHAR2 (100);
      L_Line_count         NUMBER := 1;
      L_document           BLOB;
      L_unit               s_datatype.unit_name := 'fra_invoice.p_non_merch';
      L_apex_security_id   NUMBER;
      L_file_name          VARCHAR2 (100)
         := SUBSTR (I_blob_name, INSTR (I_blob_name, '/') + 1);
   BEGIN
      -- set up security
      SELECT MAX (workspace_id) INTO L_apex_security_id FROM apex_workspaces;

      wwv_flow_api.set_security_group_id (L_apex_security_id);

      -- clear out previous spreadsheet entries for partner
      DELETE FROM w_fra_non_merch_invoice
            WHERE partner_id = I_partner_id AND file_name = L_file_name;

      -- Get blob to process
      SELECT blob_content
        INTO L_document
        FROM wwv_flow_files
       WHERE name = I_blob_name;

      L_blob_length := DBMS_LOB.getlength (L_document);


      -- Read and convert raw to varchar
      WHILE (L_position <= L_blob_length)
      LOOP
         L_raw_buffer :=
            DBMS_LOB.SUBSTR (L_document, L_chunk_size, L_position);
         L_char_buffer := UTL_RAW.cast_to_varchar2 (L_raw_buffer);

         L_position := L_position + L_chunk_size;

         -- When a whole line is retrieved
         IF INSTR (L_char_buffer, CHR (10)) > 0
         THEN
            L_line :=
                  L_line
               || SUBSTR (L_char_buffer,
                          1,
                          INSTR (L_char_buffer, CHR (13)) - 1);

            -- process line, first line is for headers so ignore
            IF INSTR (L_line, 'STANDARD EQUIPMENT SCHEDULE') > 0
            THEN
               -- RI invoice number, extract
               INSERT INTO w_fra_non_merch_invoice (file_name,
                                                    partner_id,
                                                    invoice_number,
                                                    invoice_type)
                       VALUES (
                                 L_file_name,
                                 I_partner_id,
                                 REPLACE (
                                    s_delim_str.f_get_string (L_line, 1, ','),
                                    'STANDARD EQUIPMENT SCHEDULE ',
                                    NULL),
                                 'RI');
            END IF;

            IF     L_line_count >= 4
               AND s_delim_str.f_get_string (L_line, 8, ',') != 'TOTAL'
            THEN
               L_line := REPLACE (L_line, CHR (13), NULL); -- strip carriage returns
               L_line := REPLACE (L_Line, CHR (163), NULL); -- strip currency sign

               -- remove commas from numbers in quotes, then remove quotes
               IF INSTR (L_line, '"') > 0
               THEN
                  L_line :=
                        SUBSTR (L_line, 1, INSTR (L_line, '"', 1) - 1)
                     || REPLACE (SUBSTR (L_Line,
                                         INSTR (L_line, '"', 1),
                                           INSTR (L_line,
                                                  '"',
                                                  1,
                                                  2)
                                         - INSTR (L_line,
                                                  '"',
                                                  1,
                                                  1)),
                                 ',',
                                 NULL)
                     || SUBSTR (L_line,
                                INSTR (L_line,
                                       '"',
                                       1,
                                       2));
               END IF;

               L_line := REPLACE (L_line, '"', NULL);
               L_line :=
                  REPLACE (REPLACE (L_line, ', ,', ',0,'), ', ,', ',0,');

               DBMS_OUTPUT.put_line (L_line);

               -- decompose delimited line to compoments
               -- and insert into table

               BEGIN
                  INSERT INTO w_fra_non_merch_invoice (file_name,
                                                       partner_id,
                                                       location_id,
                                                       reference,
                                                       product_spec,
                                                       composition,
                                                       comm_code,
                                                       country_code,
                                                       box_quantity,
                                                       unit_cost,
                                                       quantity,
                                                       total_cost,
                                                       box_number,
                                                       gross_weight,
                                                       net_weight,
                                                       manufacturer_ref_num,
                                                       invoice_number,
                                                       invoice_type,
                                                       created_dtm,
                                                       last_changed_dtm,
                                                       created_by,
                                                       last_changed_by)
                          VALUES (
                                    L_file_name,
                                    I_partner_id,
                                    TO_NUMBER (
                                       s_delim_str.f_get_string (L_line,
                                                                 16,
                                                                 ',')), -- location id
                                    s_delim_str.f_get_string (L_line, 1, ','), -- reference
                                    s_delim_str.f_get_string (L_line, 2, ','), -- product spec
                                    s_delim_str.f_get_string (L_line, 3, ','), -- composition
                                    s_delim_str.f_get_string (L_line, 4, ','), -- comm code
                                    s_delim_str.f_get_string (L_line, 5, ','), --  country code
                                    TO_NUMBER (
                                       NVL (
                                          s_delim_str.f_get_string (L_line,
                                                                    6,
                                                                    ','),
                                          '0')),               -- box quantity
                                    TO_NUMBER (
                                       NVL (
                                          s_delim_str.f_get_string (L_line,
                                                                    7,
                                                                    ','),
                                          0)),                    -- unit cost
                                    TO_NUMBER (
                                       NVL (
                                          s_delim_str.f_get_string (L_line,
                                                                    8,
                                                                    ','),
                                          0)),                    --  quantity
                                    TO_NUMBER (
                                       NVL (
                                          s_delim_str.f_get_string (L_line,
                                                                    9,
                                                                    ','),
                                          0)),                   -- total cost
                                    TO_NUMBER (
                                       NVL (
                                          s_delim_str.f_get_string (L_line,
                                                                    10,
                                                                    ','),
                                          0)),                   -- box number
                                    TO_NUMBER (
                                       NVL (
                                          s_delim_str.f_get_string (L_line,
                                                                    11,
                                                                    ','),
                                          0)),                 -- gross weight
                                    TO_NUMBER (
                                       NVL (
                                          s_delim_str.f_get_string (L_line,
                                                                    12,
                                                                    ','),
                                          0)),                   -- net weight
                                    s_delim_str.f_get_string (L_line,
                                                              13,
                                                              ','), -- manufacturer ref num
                                    s_delim_str.f_get_string (L_line,
                                                              14,
                                                              ','), -- invoice number
                                    s_delim_str.f_get_string (L_line,
                                                              15,
                                                              ','), --  invoice type
                                    SYSDATE,                    -- created dtm
                                    SYSDATE,               -- last changed dtm
                                    I_user,                      -- created by
                                    I_user                  -- last updated by
                                          );
               EXCEPTION
                  WHEN OTHERS
                  THEN
                     DBMS_OUTPUT.put_line ('ERROR ' || L_line);

                     RAISE;
               END;
            END IF;                                               -- Clear out

            L_line :=
               SUBSTR (L_char_buffer, INSTR (L_char_buffer, CHR (10)) + 1);
            L_char_buffer := NULL;
            L_line_count := L_line_count + 1;
         ELSE
            L_line := L_line || L_char_buffer;
         END IF;
      END LOOP;
   END p_upload_non_merch;

   --
   -- UNIT:        p_create_non_merch_invoice
   -- DESCRIPTION: Create invoice for passed partner from uploaded non
   --              merch invoice data
   -- NOTES:       If the invoice number can already be found, then this is returned back to the 
   --              calling routine. 
   PROCEDURE p_create_non_merch_invoice (
      I_partner_id          IN     fra_partner.partner_id%TYPE,
      I_user                IN     s_datatype.user_id,
      I_filename            IN     w_fra_non_merch_invoice.file_name%TYPE,
      O_invoice_header_id   OUT    fra_invoice_header.invoice_header_id%TYPE)
   IS
      L_unit                s_datatype.unit_name
                               := 'fra_invoice.p_create_non_merch_invoice';
      L_invoice_header_id   fra_invoice_header.invoice_header_id%TYPE := NULL;
      L_invoice_number      fra_invoice_header.invoice_number%TYPE;
   BEGIN
       -- Get current invoice number
       SELECT invoice_number
       INTO   L_invoice_number
       FROM   w_fra_non_merch_invoice
       WHERE  partner_id   = I_partner_id
       AND    invoice_type = 'RI'
       AND    file_name    = I_filename;

       -- Check to see if current invoice_number has been created
       BEGIN
           SELECT fih.invoice_header_id
           INTO   L_invoice_header_id
           FROM   w_fra_non_merch_invoice wi
                  JOIN fra_invoice_header fih
                      ON (fih.invoice_number = wi.invoice_number)
           WHERE  wi.partner_id   = I_partner_id
           AND    wi.invoice_type = 'RI'
           AND    wi.file_name    = I_filename;
       EXCEPTION
           WHEN NO_DATA_FOUND
           THEN
               NULL;
       END;

       IF L_invoice_header_id IS NULL
       THEN
           -- Invoice hasn't been created yet
           -- Create fra_invoice_header entries

           -- Get the invoice primary key from the sequence

           L_invoice_header_id := fra_invoice_header_seq.NEXTVAL;
           
           DBMS_OUTPUT.put_line ('L_invoice_header_id: ' || TO_CHAR (L_invoice_header_id));

           INSERT INTO fra_invoice_header 
               (invoice_header_id,
                invoice_number,
                partner_id,
                contact_id,
                invoice_total,
                uplift_total,
                invoice_gross_weight,
                invoice_net_weight,
                invoice_notes_1,
                invoice_notes_2,
                gl_extract_dtm,
                invoiced_date,
                created_dtm,
                last_changed_dtm,
                created_by,
                last_changed_by,
                ship_to_contact_id,
                non_merchandise_ivc_ind)
           SELECT L_invoice_header_id,
                  L_invoice_number,
                  I_partner_id,
                  NULL,                    -- contact id not known
                  SUM (total_cost),
                  0,                       -- no uplift on source invoice
                  SUM (gross_weight),
                  SUM (net_weight),
                  NULL,                    -- no notes on source invoice
                  NULL,                    -- no notes on source invoice
                  NULL,                    -- GL not yet extracted
                  NULL,                    -- Invoice not yet sent to partner
                  SYSDATE,
                  SYSDATE,
                  I_user,
                  I_user,
                  NULL,                    -- shipping contact not known
                  s_const.C_true           -- Non mechandise invoice indicator
           FROM   w_fra_non_merch_invoice
           WHERE  partner_id    = I_partner_id
           AND    invoice_type != 'RI'
           AND    file_name     = I_filename;

           -- create fra_invoice_line entries
           INSERT INTO fra_invoice_line 
               (invoice_line_id,
                invoice_header_id,
                carton_number,
                to_loc,
                quantity,
                trf_carton_weight,
                trf_carton_net_weight,
                original_price,
                invoice_price,
                uplift_amount,
                retail_descr,
                product_descr,
                look_descr,
                source_code,
                source_name,
                tariff_number,
                composition_percent_1,
                composition_descr_1,
                created_dtm,
                last_changed_dtm,
                created_by,
                last_changed_by)
           SELECT fra_invoice_line_seq.NEXTVAL,
                  L_invoice_header_id,
                  box_number,
                  location_id,
                  box_quantity,
                  gross_weight,
                  net_weight,
                  unit_cost,
                  unit_cost,
                  0,                                       -- no uplift
                  SUBSTR (product_spec, 1, 40),
                  SUBSTR (product_spec, 1, 40),
                  SUBSTR (product_spec, 1, 40),
                  country_code,
                  country_code,
                  comm_code,
                  100,                                     -- composition percentage
                  composition,
                  SYSDATE,
                  SYSDATE,
                  I_user,
                  I_user
           FROM   w_fra_non_merch_invoice
           WHERE  partner_id    = I_partner_id
           AND    invoice_type != 'RI'
           AND    file_name     = I_filename;
       END IF;

       O_invoice_header_id := L_invoice_header_id;
--   EXCEPTION
--   WHEN OTHERS
--   THEN
--       s_err.p_error_handler;
   END p_create_non_merch_invoice;
END fra_invoice;
/
SHOW ERRORS
