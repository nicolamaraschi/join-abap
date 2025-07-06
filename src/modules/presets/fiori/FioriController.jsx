
export const content = `
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/m/MessageToast",
    "sap/ui/model/json/JSONModel"
], function (Controller, MessageToast, JSONModel) {
    "use strict";

    return Controller.extend("my.namespace.controller.BaseController", {

        onInit: function () {
            // Initialization logic here
            var oViewModel = new JSONModel({
                isEditMode: false
            });
            this.getView().setModel(oViewModel, "viewModel");
        },

        onEditPress: function () {
            this.getView().getModel("viewModel").setProperty("/isEditMode", true);
            MessageToast.show("Edit mode enabled.");
        },

        onSavePress: function () {
            // Add save logic here
            this.getView().getModel("viewModel").setProperty("/isEditMode", false);
            MessageToast.show("Changes saved successfully.");
        },

        onNavBack: function () {
            var oHistory = sap.ui.core.routing.History.getInstance();
            var sPreviousHash = oHistory.getPreviousHash();

            if (sPreviousHash !== undefined) {
                window.history.go(-1);
            } else {
                var oRouter = this.getOwnerComponent().getRouter();
                oRouter.navTo("listReport", {}, true);
            }
        }
    });
});
`;
