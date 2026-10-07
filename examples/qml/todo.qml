// UI for examples/qml_todo.kayte - the QML only draws; the Kayte script
// owns the logic. Objects the script talks to have an objectName.
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

ApplicationWindow {
    id: root
    objectName: "root"
    title: "Kayte + QML To-Do"
    width: 400
    height: 460

    // Kayte reads/writes this like any property: QT "setprop", win, "status", "..."
    property string status: "No tasks yet"

    // Emitted by a row's ✕ button; Kayte gets the row via QT "eventarg".
    signal removeRequested(int index)

    // Called from Kayte with QT "call", win, "addTask", text
    function addTask(text) {
        tasks.append({ "title": text, "done": false })
        return tasks.count
    }
    function removeTask(index) {
        tasks.remove(index)
        return tasks.count
    }
    function clearTasks() {
        tasks.clear()
    }

    ListModel { id: tasks }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        spacing: 8

        RowLayout {
            Layout.fillWidth: true
            TextField {
                id: entry
                objectName: "entry"
                Layout.fillWidth: true
                placeholderText: "What needs doing?"
            }
            Button {
                objectName: "addButton"
                text: "Add"
            }
        }

        ListView {
            Layout.fillWidth: true
            Layout.fillHeight: true
            clip: true
            model: tasks
            spacing: 4
            delegate: Rectangle {
                required property int index
                required property string title
                width: ListView.view.width
                height: 40
                radius: 6
                color: index % 2 ? "#f2f4f8" : "#e6ebf5"
                CheckBox {
                    id: check
                    anchors.left: parent.left
                    anchors.verticalCenter: parent.verticalCenter
                }
                Label {
                    anchors.left: check.right
                    anchors.right: removeBtn.left
                    anchors.verticalCenter: parent.verticalCenter
                    text: title
                    elide: Text.ElideRight
                    font.strikeout: check.checked
                    opacity: check.checked ? 0.5 : 1
                }
                ToolButton {
                    id: removeBtn
                    anchors.right: parent.right
                    anchors.verticalCenter: parent.verticalCenter
                    text: "✕"
                    onClicked: root.removeRequested(index)
                }
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Label {
                Layout.fillWidth: true
                text: root.status
            }
            Button {
                objectName: "clearButton"
                text: "Clear all"
            }
        }
    }
}
