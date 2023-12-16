$(document).ready(function () {
    $(".protected-email").each(function (i,v) {
        var item = $(this);
        var email = item.data('email');
        item.attr('href', 'mailto:' + email);
        item.html(email);
    });
});
