function playCard(element) {
  Shiny.setInputValue('played', element.getAttribute('index'), { priority: 'event' });
}