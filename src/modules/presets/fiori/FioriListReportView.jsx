
export const content = `
<mvc:View
    controllerName="my.namespace.controller.ListReport"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m"
    xmlns:smartFilterBar="sap.ui.comp.smartfilterbar"
    xmlns:smartTable="sap.ui.comp.smarttable">

    <Page id="listReportPage" title="{i18n>listReportTitle}">
        <content>
            <smartFilterBar:SmartFilterBar
                id="smartFilterBar"
                entitySet="MyEntitySet"
                persistencyKey="myPersistencyKey">
                <!-- Custom Filters can be added here -->
            </smartFilterBar:SmartFilterBar>

            <smartTable:SmartTable
                id="smartTable"
                entitySet="MyEntitySet"
                smartFilterId="smartFilterBar"
                tableType="ResponsiveTable"
                useExportToExcel="true"
                useVariantManagement="true"
                useTablePersonalisation="true"
                header="Line Items"
                showRowCount="true"
                persistencyKey="mySmartTablePersistencyKey"
                enableAutoBinding="true"
                class="sapUiResponsiveContentPadding">
                <!-- Custom Columns can be added here -->
            </smartTable:SmartTable>
        </content>
    </Page>
</mvc:View>
`;
