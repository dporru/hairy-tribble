$(document).ready(function(){
    // List with handle
    Sortable.create(listWithHandle, {
        handle: '.glyphicon-move',
        animation: 150
    });
});