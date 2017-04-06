package cn.blmdz.boot.datasource.autoconfigure;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import org.springframework.util.Assert;

public class DriverClassNameProvider {
   private static final String JDBC_URL_PREFIX = "jdbc";
   private static final Map<String, String> DRIVERS;

   String getDriverClassName(String jdbcUrl) {
      Assert.notNull(jdbcUrl, "JdbcUrl must not be null");
      Assert.isTrue(jdbcUrl.startsWith("jdbc"), "JdbcUrl must start with \'jdbc\'");
      String urlWithoutPrefix = jdbcUrl.substring("jdbc".length());

      for(Entry<String, String> driver : DRIVERS.entrySet()) {
         if(urlWithoutPrefix.startsWith(":" + (String)driver.getKey() + ":")) {
            return (String)driver.getValue();
         }
      }

      return null;
   }

   static {
      Map<String, String> drivers = new HashMap();
      drivers.put("derby", "org.apache.derby.jdbc.EmbeddedDriver");
      drivers.put("h2", "org.h2.Driver");
      drivers.put("hsqldb", "org.hsqldb.jdbc.JDBCDriver");
      drivers.put("sqlite", "org.sqlite.JDBC");
      drivers.put("mysql", "com.mysql.jdbc.Driver");
      drivers.put("mariadb", "org.mariadb.jdbc.Driver");
      drivers.put("google", "com.google.appengine.api.rdbms.AppEngineDriver");
      drivers.put("oracle", "oracle.jdbc.OracleDriver");
      drivers.put("postgresql", "org.postgresql.Driver");
      drivers.put("jtds", "net.sourceforge.jtds.jdbc.Driver");
      drivers.put("sqlserver", "com.microsoft.sqlserver.jdbc.SQLServerDriver");
      drivers.put("firebirdsql", "org.firebirdsql.jdbc.FBDriver");
      drivers.put("db2", "com.ibm.db2.jcc.DB2Driver");
      drivers.put("teradata", "com.teradata.jdbc.TeraDriver");
      DRIVERS = Collections.unmodifiableMap(drivers);
   }
}
