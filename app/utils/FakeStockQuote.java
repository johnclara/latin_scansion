package utils;

public class FakeStockQuote implements StockQuote {

    public String newParse(String lastParse) {
        if (Character.isLowerCase(lastParse.charAt(0))){
          return lastParse.toUpperCase();
        } else {
          return lastParse.toLowerCase();
        }
    }

}
