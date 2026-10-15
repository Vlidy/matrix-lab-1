import java.util.*;
public class My_lab {
    public static void main(String args[]){
        Scanner vv = new Scanner(System.in);
        vv.useLocale(Locale.US);
        System.out.println("Введите матрицу A:");
        ComplexMatrix a = ComplexMatrix.getMatrix(vv);
        int choice = -1;
        while(choice!=0){
            System.out.println();
            System.out.println("Выберите операцию: 1-сложить, 2-умножить, 3-разделить, 4-транспонировать, 5-определитель, 6-ввести новую матрицу A, 0-выход");
            choice = ComplexMatrix.getInt(vv);
            try{
                switch(choice){
                    case 1:
                    case 2:
                    case 3:
                        System.out.println("введите матрицу B:");
                        ComplexMatrix b = ComplexMatrix.getMatrix(vv);
                        ComplexMatrix res;
                        if(choice==1){
                            res = a.sum(b);
                        }
                        else if(choice==2){
                            res = a.multi(b);
                        }
                        else{
                            res = a.div(b);
                        }
                        res.printMatrix();
                        System.out.println();
                        break;
                    case 4:
                        a.transp().printMatrix();
                        System.out.println();
                        break;
                    case 5:
                        Complex d = a.det();
                        System.out.println("("+d.rPart+"+"+d.mPart+"i)");
                        break;
                    case 6:
                        a = ComplexMatrix.getMatrix(vv);
                        break;
                    case 0:
                        break;
                    default:
                        System.out.println("некорректная операция");
                }
            }
            catch(IllegalArgumentException e){
                System.out.println(e.getMessage());
            }
        }
    }
}
class Complex{
    double rPart;
    double mPart;
    Complex(double rPart,double mPart){
        this.rPart=rPart;
        this.mPart=mPart;
    }
    Complex sumComplex(Complex num){
        Complex res= new Complex(this.rPart+num.rPart,this.mPart+ num.mPart);
        return res;
    }
    Complex subComplex(Complex num){
        Complex res= new Complex(this.rPart-num.rPart,this.mPart-num.mPart);
        return res;
    }
    Complex multComplex(Complex num){
        Complex res1= new Complex(this.rPart* num.rPart-this.mPart*num.mPart,this.mPart*num.rPart+this.rPart*num.mPart);
        return res1;
    }
    Complex divComplex(Complex num){
        if(num.is0()){
            throw new IllegalArgumentException("Ошибка:деление на ноль");
        }
        double z = num.rPart*num.rPart+num.mPart*num.mPart;
        Complex res2= new Complex((this.rPart*num.rPart+this.mPart*num.mPart)/z,(this.mPart*num.rPart-this.rPart*num.mPart)/z);
        return res2;
    }
    double modSq(){
        return this.rPart*this.rPart+this.mPart*this.mPart;
    }
    boolean is0(){
        return this.modSq()<1e-18;
    }

}
class ComplexMatrix implements Operation{
    int str;
    int stl;
    Complex[][] mat;
    ComplexMatrix(int str, int stl){
        this.str=str;
        this.stl=stl;
        this.mat=new Complex[str][stl];
    }
    private ComplexMatrix getMinor(int stolb){
        ComplexMatrix minor1 = new ComplexMatrix(this.str-1,this.stl-1);
        for(int i=1;i<this.str;i++){
            int minorj=0;
            for(int j=0;j<this.stl;j++){
                if(j==stolb){
                    continue;
                }
                minor1.mat[i-1][minorj]=this.mat[i][j];
                minorj++;
            }
        }
        return minor1;
    }
    private ComplexMatrix getInverse(){
        if (!this.isSquare()) {
            throw new IllegalArgumentException("Ошибка: обратную матрицу нельзя посчитвть для неквадратной матрицы!");
        }
        ComplexMatrix a = new ComplexMatrix(this.str,this.stl);
        ComplexMatrix inv = new ComplexMatrix(this.str,this.stl);
        for(int i=0;i<this.str;i++){
            for(int j=0;j<this.stl;j++){
                a.mat[i][j]=this.mat[i][j];
                if(i==j){
                    inv.mat[i][j]=new Complex(1,0);
                }
                else{
                    inv.mat[i][j]=new Complex(0,0);
                }
            }
        }
        for(int stolb=0;stolb<this.str;stolb++){
            int ptr=stolb;
            for(int i=stolb+1;i<this.str;i++){
                if(a.mat[i][stolb].modSq()>a.mat[ptr][stolb].modSq()){
                    ptr=i;
                }
            }
            if(a.mat[ptr][stolb].is0()){
                throw new IllegalArgumentException("Ошибка:определитель равен нулю,значит обратной матрицы нет");
            }
            Complex[] tmp=a.mat[stolb];
            a.mat[stolb]=a.mat[ptr];
            a.mat[ptr]=tmp;
            tmp=inv.mat[stolb];
            inv.mat[stolb]=inv.mat[ptr];
            inv.mat[ptr]=tmp;
            Complex p=a.mat[stolb][stolb];
            for(int j=0;j<this.stl;j++){
                a.mat[stolb][j]=a.mat[stolb][j].divComplex(p);
                inv.mat[stolb][j]=inv.mat[stolb][j].divComplex(p);
            }
            for(int i=0;i<this.str;i++){
                if(i==stolb){
                    continue;
                }
                Complex f=a.mat[i][stolb];
                for(int j=0;j<this.stl;j++){
                    a.mat[i][j]=a.mat[i][j].subComplex(f.multComplex(a.mat[stolb][j]));
                    inv.mat[i][j]=inv.mat[i][j].subComplex(f.multComplex(inv.mat[stolb][j]));
                }
            }
        }
        return inv;
    }
    public boolean equalSize(ComplexMatrix x){
        if(this.str==x.str && this.stl==x.stl){
            return true;
        }
        else{
            return false;
        }
    }
    public ComplexMatrix sum(ComplexMatrix x){
        if (this.equalSize(x)){
            ComplexMatrix res= new ComplexMatrix(this.str,this.stl);
            for(int stri=0;stri< str;stri++){
                for(int stli=0;stli<stl;stli++){
                    res.mat[stri][stli]=this.mat[stri][stli].sumComplex(x.mat[stri][stli]);
                }
            }
            return res;
        }
        else{
            throw new IllegalArgumentException("ошибка:нельзя складывать матрицы разных размеров");
        }
    }
    public boolean isSquare(){
        if(this.stl==this.str){
            return true;
        }
        else{
            return false;
        }
    }
    public ComplexMatrix transp(){
        ComplexMatrix trMat = new ComplexMatrix(this.stl, this.str);
        for (int stri = 0; stri < str; stri++) {
            for (int stli = 0; stli < stl; stli++) {
                trMat.mat[stli][stri] = this.mat[stri][stli];
            }
        }
        return trMat;

    }
    public ComplexMatrix multi(ComplexMatrix x){
        if(this.stl==x.str){
            ComplexMatrix results =  new ComplexMatrix(this.str,x.stl);
            for (int stri = 0; stri < this.str; stri++) {
                for (int stli = 0; stli < x.stl; stli++) {
                    Complex locsum= new Complex(0,0);
                    for (int k=0;k<this.stl;k++){
                        Complex p =this.mat[stri][k].multComplex(x.mat[k][stli]);
                        locsum=locsum.sumComplex(p);
                    }
                    results.mat[stri][stli]=locsum;
                }
            }
            return results;
        }
        else{
            throw new IllegalArgumentException("ошибка: нельзя умножать матрицы разных размеров");
        }

    }
    public ComplexMatrix div(ComplexMatrix x){
        if(this.stl==x.str && x.isSquare()){
            return this.multi(x.getInverse());
        }
        else{
            throw new IllegalArgumentException("Ошибка: нельзя делить матрицы разных размеров");
        }
    }
    public Complex det(){
        if (!this.isSquare()) {
            throw new IllegalArgumentException("Ошибка: Определитель можно посчитать только у квадратной матрицы!");
        }
        if (this.str==1){
            return this.mat[0][0];
        }
        if(this.str==2){
            Complex a= this.mat[0][0].multComplex(this.mat[1][1]);
            Complex b= this.mat[0][1].multComplex(this.mat[1][0]);
            return a.subComplex(b);
        }
        Complex det1 = new Complex(0, 0);
        for (int j = 0; j < this.stl; j++) {
            Complex elem = this.mat[0][j];
            Complex minorDet = getMinor(j).det();
            Complex t = elem.multComplex(minorDet);
            if (j % 2 == 1) {
                det1 = det1.subComplex(t);
            } else {
                det1 = det1.sumComplex(t);
            }
        }

        return det1;
    }
    public void printMatrix() {
        for(int i=0;i<this.str;i++){
            System.out.println();
            for(int j=0;j<this.stl;j++){
                Complex  curr=this.mat[i][j];
                System.out.print("("+curr.rPart+"+"+curr.mPart+"i)");
            }
        }
    }
    public static int getInt(Scanner vv) {
        while(!vv.hasNextInt()){
            System.out.print("Ошибка:нужно ввести целое число,повторите ввод: ");
            vv.next();
        }
        return vv.nextInt();
    }
    public static double getDouble(Scanner vv) {
        while(!vv.hasNextDouble()){
            System.out.print("Ошибка: нужно ввести число,а дробную часть через точку повторите ввод: ");
            vv.next();
        }
        return vv.nextDouble();
    }
    public static ComplexMatrix getMatrix(Scanner vv) {
        System.out.print("Введите количество строк  матрицы:");
        int locstr = getInt(vv);
        while(locstr<=0){
            System.out.print("Ошибка: число должно быть больше нуля. Повторите ввод: ");
            locstr = getInt(vv);
        }
        System.out.print("Введите количество столбцов матрицы: ");
        int locstl = getInt(vv);
        while(locstl<=0){
            System.out.print("Ошибка: число должно быть больше нуля. Повторите ввод: ");
            locstl = getInt(vv);
        }
        ComplexMatrix matrix = new ComplexMatrix(locstr, locstl);
        System.out.println("Заполнение элементов (вводите реальную часть  и минмую часть числа через пробел):");
        for (int i = 0; i < locstr; i++) {
            for (int j = 0; j < locstl; j++) {
                System.out.print("Элемент [" + i + "][" + j + "] : ");
                double rp = getDouble(vv);
                double mp = getDouble(vv);
                matrix.mat[i][j] = new Complex(rp, mp);
            }
        }
        return matrix;
    }
}
interface Operation {
    ComplexMatrix transp();
    ComplexMatrix sum(ComplexMatrix x);
    ComplexMatrix multi(ComplexMatrix x);
    ComplexMatrix div(ComplexMatrix x);
    Complex det();
    boolean equalSize(ComplexMatrix x);
    boolean isSquare();
    void printMatrix();

}