
export const content = `
<mvc:View
    controllerName="my.namespace.controller.ObjectPage"
    xmlns="sap.uxap"
    xmlns:m="sap.m"
    xmlns:f="sap.ui.layout.form"
    xmlns:mvc="sap.ui.core.mvc">

    <ObjectPageLayout id="objectPageLayout">
        <headerTitle>
            <ObjectPageHeader
                objectTitle="{Name}"
                objectSubtitle="{Description}">
                <actions>
                    <ObjectPageHeaderActionButton icon="sap-icon://edit" text="Edit" press=".onEditPress" />
                    <ObjectPageHeaderActionButton icon="sap-icon://save" text="Save" press=".onSavePress" />
                </actions>
            </ObjectPageHeader>
        </headerTitle>

        <sections>
            <ObjectPageSection title="General Information">
                <subSections>
                    <ObjectPageSubSection>
                        <blocks>
                            <f:SimpleForm
                                maxContainerCols="2"
                                editable="false"
                                layout="ResponsiveGridLayout"
                                labelSpanL="3"
                                labelSpanM="3"
                                emptySpanL="4"
                                emptySpanM="4"
                                columnsL="1"
                                columnsM="1">
                                <f:content>
                                    <m:Label text="ID" />
                                    <m:Text text="{ID}" />
                                    <m:Label text="Status" />
                                    <m:Text text="{Status}" />
                                </f:content>
                            </f:SimpleForm>
                        </blocks>
                    </ObjectPageSubSection>
                </subSections>
            </ObjectPageSection>

            <ObjectPageSection title="Details">
                <subSections>
                    <ObjectPageSubSection>
                        <blocks>
                            <!-- Additional content here -->
                        </blocks>
                    </ObjectPageSubSection>
                </subSections>
            </ObjectPageSection>
        </sections>
    </ObjectPageLayout>
</mvc:View>
`;
