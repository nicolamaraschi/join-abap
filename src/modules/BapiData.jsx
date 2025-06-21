// --- FI BAPIs ---
import { bapi as BAPI_ACC_DOCUMENT_POST } from './bapis/FiAccDocumentPost.jsx';
import { bapi as BAPI_AP_ACC_GETOPENITEMS } from './bapis/FiApAccGetOpenItems.jsx';
import { bapi as BAPI_AR_ACC_GETOPENITEMS } from './bapis/FiArAccGetOpenItems.jsx';
import { bapi as BAPI_BANK_CREATE } from './bapis/FiBankCreate.jsx';
import { bapi as BAPI_FIXEDASSET_CREATE1 } from './bapis/FiFixedAssetCreate1.jsx';

// --- SD BAPIs ---
import { bapi as BAPI_SALESORDER_CREATEFROMDAT2 } from './bapis/SdSalesOrderCreateFromDat2.jsx';
import { bapi as BAPI_SALESORDER_CHANGE } from './bapis/SdSalesOrderChange.jsx';
import { bapi as BAPI_SALESORDER_GETSTATUS } from './bapis/SdSalesOrderStatus.jsx';
import { bapi as BAPI_BILLINGDOC_CREATEMULTIPLE } from './bapis/SdBillingDocCreateMultiple.jsx';
import { bapi as BAPI_OUTB_DELIVERY_CREATE_SLS } from './bapis/SdOutbDeliveryCreateSls.jsx';
import { bapi as BAPI_CUSTOMERRETURN_CREATE } from './bapis/SdCustomerReturnCreate.jsx';

// --- MM BAPIs ---
import { bapi as BAPI_PO_CREATE1 } from './bapis/MmPoCreate1.jsx';
import { bapi as BAPI_GOODSMVT_CREATE_MM } from './bapis/MmGoodsMvtCreate.jsx';
import { bapi as BAPI_MATERIAL_SAVEDATA } from './bapis/MmMaterialSaveData.jsx';
import { bapi as BAPI_REQUISITION_CREATE } from './bapis/MmRequisitionCreate.jsx';
import { bapi as BAPI_INCOMINGINVOICE_CREATE } from './bapis/MmIncomingInvoiceCreate.jsx';
import { bapi as BAPI_RESERVATION_CREATE1 } from './bapis/MmReservationCreate1.jsx';

// --- PP BAPIs ---
import { bapi as BAPI_PRODORD_CREATE } from './bapis/PpProdOrdCreate.jsx';
import { bapi as BAPI_PRODORD_RELEASE } from './bapis/PpProdOrdRelease.jsx';
import { bapi as BAPI_PRODORD_COMPLETE_TECH } from './bapis/PpProdOrdCompleteTech.jsx';
import { bapi as BAPI_GOODSMVT_CREATE_PP } from './bapis/PpGoodsMvtCreate.jsx';
import { bapi as BAPI_GOODSMVT_CANCEL } from './bapis/PpGoodsMvtCancel.jsx';
import { bapi as BAPI_PLANNEDORDER_CREATE } from './bapis/PpPlannedOrderCreate.jsx';

// --- PM BAPIs ---
import { bapi as BAPI_ALM_ORDER_MAINTAIN } from './bapis/PmAlmOrderMaintain.jsx';
import { bapi as BAPI_EQUI_CREATE } from './bapis/PmEquiCreate.jsx';
import { bapi as EAM_TASKLIST_CREATE } from './bapis/PmEamTaskListCreate.jsx';

// --- CO BAPIs ---
import { bapi as BAPI_COSTCENTER_CREATEMULTIPLE } from './bapis/CoCostCenterCreateMultiple.jsx';
import { bapi as BAPI_INTERNALORDER_CREATE } from './bapis/CoInternalOrderCreate.jsx';
import { bapi as BAPI_PROFITCENTER_CREATE } from './bapis/CoProfitCenterCreate.jsx';

// --- BP BAPIs ---
import { bapi as BAPI_BUPA_CREATE_FROM_DATA } from './bapis/BpBupaCreateFromData.jsx';

// --- PS BAPIs ---
import { bapi as BAPI_PROJECT_MAINTAIN } from './bapis/PsProjectMaintain.jsx';

// --- QM BAPIs ---
import { bapi as BAPI_INSPLOT_CREATE } from './bapis/QmInsplotCreate.jsx';
import { bapi as BAPI_QUALNOT_CREATE } from './bapis/QmQualnotCreate.jsx';


export const bapiData = {
    "FI": [
        BAPI_ACC_DOCUMENT_POST,
        BAPI_AP_ACC_GETOPENITEMS,
        BAPI_AR_ACC_GETOPENITEMS,
        BAPI_BANK_CREATE,
        BAPI_FIXEDASSET_CREATE1
    ],
    "SD": [
        BAPI_SALESORDER_CREATEFROMDAT2,
        BAPI_SALESORDER_CHANGE,
        BAPI_SALESORDER_GETSTATUS,
        BAPI_BILLINGDOC_CREATEMULTIPLE,
        BAPI_OUTB_DELIVERY_CREATE_SLS,
        BAPI_CUSTOMERRETURN_CREATE
    ],
    "MM": [
        BAPI_PO_CREATE1,
        BAPI_GOODSMVT_CREATE_MM,
        BAPI_MATERIAL_SAVEDATA,
        BAPI_REQUISITION_CREATE,
        BAPI_INCOMINGINVOICE_CREATE,
        BAPI_RESERVATION_CREATE1
    ],
    "PP": [
        BAPI_PRODORD_CREATE,
        BAPI_PRODORD_RELEASE,
        BAPI_PRODORD_COMPLETE_TECH,
        BAPI_GOODSMVT_CREATE_PP,
        BAPI_GOODSMVT_CANCEL,
        BAPI_PLANNEDORDER_CREATE
    ],
    "PM": [
        BAPI_ALM_ORDER_MAINTAIN,
        BAPI_EQUI_CREATE,
        EAM_TASKLIST_CREATE
    ],
    "CO": [
        BAPI_COSTCENTER_CREATEMULTIPLE,
        BAPI_INTERNALORDER_CREATE,
        BAPI_PROFITCENTER_CREATE
    ],
    "BP": [
        BAPI_BUPA_CREATE_FROM_DATA
    ],
    "PS": [
        BAPI_PROJECT_MAINTAIN
    ],
    "QM": [
        BAPI_INSPLOT_CREATE,
        BAPI_QUALNOT_CREATE
    ]
};