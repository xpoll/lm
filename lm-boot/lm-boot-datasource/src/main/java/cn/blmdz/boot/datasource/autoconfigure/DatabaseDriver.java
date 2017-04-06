package cn.blmdz.boot.datasource.autoconfigure;

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

public enum DatabaseDriver {
   UNKNOWN((String)null),
   DERBY("org.apache.derby.jdbc.EmbeddedDriver"),
   H2("org.h2.Driver", "org.h2.jdbcx.JdbcDataSource"),
   HSQLDB("org.hsqldb.jdbc.JDBCDriver", "org.hsqldb.jdbc.pool.JDBCXADataSource"),
   SQLITE("org.sqlite.JDBC"),
   MYSQL("com.mysql.jdbc.Driver", "org.mysql.jdbc.MySQLDataSource"),
   MARIADB("org.mariadb.jdbc.Driver", "org.mariadb.jdbc.MySQLDataSource"),
   GOOGLE("com.google.appengine.api.rdbms.AppEngineDriver"),
   ORACLE("oracle.jdbc.OracleDriver", "oracle.jdbc.xa.client.OracleXADataSource"),
   POSTGRESQL("org.postgresql.Driver", "org.postgresql.xa.PGXADataSource"),
   JTDS("net.sourceforge.jtds.jdbc.Driver"),
   SQLSERVER("com.microsoft.sqlserver.jdbc.SQLServerDriver", "com.microsoft.sqlserver.jdbc.SQLServerXADataSource"),
   DB2("com.ibm.db2.jcc.DB2Driver", "com.ibm.db2.jcc.DB2XADataSource"),
   AS400("com.ibm.as400.access.AS400JDBCDriver", "com.ibm.as400.access.AS400JDBCXADataSource");

   private final String driverClassName;
   private final String xaDataSourceClassName;

   private DatabaseDriver(String driverClassName) {
      this(driverClassName, (String)null);
   }

   private DatabaseDriver(String driverClassName, String xaDataSourceClassName) {
      this.driverClassName = driverClassName;
      this.xaDataSourceClassName = xaDataSourceClassName;
   }

   public String getDriverClassName() {
      return this.driverClassName;
   }

   public String getXaDataSourceClassName() {
      return this.xaDataSourceClassName;
   }

   public static DatabaseDriver fromJdbcUrl(String url) {
      if(StringUtils.hasLength(url)) {
         Assert.isTrue(url.startsWith("jdbc"), "URL must start with \'jdbc\'");
         String urlWithoutPrefix = url.substring("jdbc".length()).toLowerCase();

         for(DatabaseDriver driver : values()) {
            String prefix = ":" + driver.name().toLowerCase() + ":";
            if(driver != UNKNOWN && urlWithoutPrefix.startsWith(prefix)) {
               return driver;
            }
         }
      }

      return UNKNOWN;
   }
}
