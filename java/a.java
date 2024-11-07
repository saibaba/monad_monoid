
class DataAccessGateway {
	 
    private final static String PREFIX = DataAccessGateway.class.getName();

    public void doit() { System.out.println(PREFIX); }
}

public class a {
   public static void main(String[] args) {
      new DataAccessGateway().doit();
   }
}
